package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.net.URI;
import java.net.UnknownHostException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import nl.rivm.screenit.mamma.se.proxy.SeProxyApplication;
import nl.rivm.screenit.mamma.se.proxy.model.CacheProxyActie;
import nl.rivm.screenit.mamma.se.proxy.model.RequestTypeCentraal;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeRestSocketService;
import nl.rivm.screenit.mamma.se.proxy.services.SeStatusService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.ResourceAccessException;
import org.springframework.web.client.RestTemplate;

@Service
public class ProxyServiceImpl implements ProxyService
{
	private static final Logger LOG = LoggerFactory.getLogger(ProxyServiceImpl.class);

	private final Map<String, ResponseEntity> cachedResponses = new ConcurrentHashMap<>();

	@Value("${SE_REST_URL}")
	private String seRestUrl;

	@Autowired
	private SeStatusService seStatusService;

	@Autowired
	private SeRestSocketService seRestSocketService;

	private String userAgent;

	private final static int requestTimeoutInMS = 30000;

	private RestTemplate restTemplate;

	@Override
	public void init()
	{
		restTemplate = new RestTemplate();
		SimpleClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
		requestFactory.setReadTimeout(requestTimeoutInMS);
		requestFactory.setConnectTimeout(requestTimeoutInMS);
		restTemplate.setRequestFactory(requestFactory);
		seRestUrl = StringUtils.removeEnd(seRestUrl, "/") + "/api";
		userAgent = "SE-Proxy/" + SeProxyApplication.getEnvironmentInfo().getVersion();
	}

	@Override
	public RequestEntity.BodyBuilder getProxyRequestEntity(String pathPostfix, HttpMethod method)
	{
		final String url = requestUrl(pathPostfix);
		RequestEntity.BodyBuilder requestBuilder = RequestEntity.method(method, URI.create(url));
		addProxyHeaders(requestBuilder);
		return requestBuilder;
	}

	private String requestUrl(String pathPostfix)
	{
		return seRestUrl + pathPostfix;
	}

	private String requestUrl(RequestTypeCentraal requestType)
	{
		return requestUrl(requestType.getPathPostfix());
	}

	@Override
	public RequestEntity.BodyBuilder getProxyRequestEntityAccount(String pathPostfix, HttpMethod method, String accountId)
	{
		RequestEntity.BodyBuilder requestBuilder = getProxyRequestEntity(pathPostfix, method);
		if (StringUtils.isNotBlank(accountId))
		{
			requestBuilder.header("ACCOUNT_ID", accountId);
		}
		return requestBuilder;
	}

	private void addProxyHeaders(RequestEntity.BodyBuilder requestBuilder)
	{

		requestBuilder.header("User-agent", userAgent);
		requestBuilder.header("SE_CODE", seStatusService.getSeCode());
		requestBuilder.header("versie", SeProxyApplication.getEnvironmentInfo().getVersion());

		if (seStatusService.getSeCode() == null)
		{
			requestBuilder.header("PROXY_IP", seStatusService.getProxyIp());
		}
		requestBuilder.header("SE_PROXY_DATUMTIJD", DateTimeFormatter.ISO_DATE_TIME.format(DateUtil.getCurrentDateTime()));
	}

	@Override
	public void clearTestCache()
	{
		cachedResponses.clear();
		deleteRequest(RequestTypeCentraal.RESET_HUISARTSEN_CACHE);
		deleteRequest(RequestTypeCentraal.RESET_ZORGINSTELLINGEN_CACHE);
	}

	@Override
	public <T> ResponseEntity sendCachableProxyRequest(RequestEntity requestEntity, Class<T> responseType, CacheProxyActie cacheActie)
	{
		String url = requestEntity.getUrl().toString();
		if ((cacheActie == CacheProxyActie.ALTIJD_OPHALEN_ALS_ONLINE && seStatusService.isOnline()) || !cachedResponses.containsKey(url))
		{
			ResponseEntity<T> response = sendUncheckedProxyRequest(requestEntity, responseType);
			if (response.getStatusCode().equals(HttpStatus.OK))
			{
				cachedResponses.put(url, response);
			}
		}
		return cachedResponses.getOrDefault(url, new ResponseEntity(HttpStatus.GATEWAY_TIMEOUT));
	}

	@Override
	public ResponseEntity<String> getRequest(RequestTypeCentraal requestType, CacheProxyActie cacheProxyActie)
	{
		return getRequest(requestType.getPathPostfix(), cacheProxyActie);
	}

	private ResponseEntity<String> getRequest(String pathPostfix, CacheProxyActie cacheProxyActie)
	{
		RequestEntity.BodyBuilder requestBuilder = getProxyRequestEntity(pathPostfix, HttpMethod.GET);
		return sendCachableProxyRequest(requestBuilder.build(), String.class, cacheProxyActie);
	}

	private void deleteRequest(RequestTypeCentraal pathPostfix)
	{
		restTemplate.delete(requestUrl(pathPostfix));
	}

	@Override
	public ResponseEntity<String> getPlanning(LocalDate datum, CacheProxyActie cacheProxyActie)
	{
		return getRequest(RequestTypeCentraal.GET_PLANNING.getPathPostfix() + "/" + datum.format(DateTimeFormatter.ISO_DATE), cacheProxyActie);
	}

	@Override
	public void deleteOudePlanningCache()
	{
		LocalDate nu = DateUtil.getCurrentDateTime().toLocalDate();
		String planningPathPostfix = RequestTypeCentraal.GET_PLANNING.getPathPostfix();
		cachedResponses.entrySet().removeIf(entry -> {
			String url = entry.getKey();
			int indexPlanning = url.indexOf(planningPathPostfix);
			if (indexPlanning != -1)
			{
				String date = url.substring(indexPlanning + (planningPathPostfix + "/").length());
				return LocalDate.parse(date, DateTimeFormatter.ISO_DATE).isBefore(nu);
			}
			return false;
		});
	}

	@Override
	public <T> ResponseEntity<T> sendUncheckedProxyRequest(RequestEntity requestEntity, Class<T> responseType)
	{
		try
		{
			if (!seStatusService.isTestOnline() && SeProxyApplication.getEnvironmentInfo().getEnvironment().equals("Test"))
			{
				return createNoConnectionSeRestBkResponse();
			}
			return restTemplate.exchange(requestEntity, responseType);
		}
		catch (ResourceAccessException ex)
		{
			return createNoConnectionSeRestBkResponse();
		}
		catch (HttpStatusCodeException ex)
		{
			if (ex.getStatusCode() == HttpStatus.FORBIDDEN)
			{
				LOG.warn("SeGebruiker is niet geautoriseerd", ex);
				return new ResponseEntity(ex.getResponseBodyAsString(), HttpStatus.FORBIDDEN);
			}
			if (ex.getStatusCode() == HttpStatus.NOT_FOUND)
			{
				LOG.warn("Opgevraagde resource is niet gevonden", ex);
				return new ResponseEntity(ex.getResponseBodyAsString(), HttpStatus.NOT_FOUND);
			}
			if (ex.getStatusCode() == HttpStatus.PRECONDITION_FAILED)
			{
				return new ResponseEntity(ex.getResponseBodyAsString(), HttpStatus.PRECONDITION_FAILED);
			}
			if (ex.getStatusCode() == HttpStatus.CONFLICT)
			{
				return new ResponseEntity(ex.getResponseBodyAsString(), HttpStatus.CONFLICT);
			}
			if (ex.getStatusCode() == HttpStatus.SERVICE_UNAVAILABLE)
			{
				return createNoConnectionSeRestBkResponse();
			}
			LOG.error("Bericht werd niet goed verwerkt door de centrale server: ", ex);
			return new ResponseEntity(ex.getResponseBodyAsString(), HttpStatus.INTERNAL_SERVER_ERROR);
		}

		catch (Exception e)
		{
			if (e.getCause() != null && e.getCause().getCause() instanceof UnknownHostException)
			{
				return createNoConnectionSeRestBkResponse();
			}
			LOG.error("Bericht werd niet goed verwerkt door de centrale server: Exception: ", e);
			return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	private ResponseEntity createNoConnectionSeRestBkResponse()
	{
		LOG.warn("Kon geen verbinding maken met SE-REST-BK");
		seRestSocketService.sluitSocketEnSetRandomRetry();
		return new ResponseEntity<>(HttpStatus.GATEWAY_TIMEOUT);
	}

	@Override
	public boolean huisartsenInCache()
	{
		return cachedResponses.containsKey(requestUrl(RequestTypeCentraal.GET_HUISARTSEN));
	}

	@Override
	public boolean zorginstellingeninCache()
	{
		return cachedResponses.containsKey(requestUrl(RequestTypeCentraal.GET_ZORGINSTELLINGEN));
	}

	@Override
	public boolean mammografenInCache()
	{
		return cachedResponses.containsKey(requestUrl(RequestTypeCentraal.GET_MAMMOGRAFEN));
	}

	@Override
	public String cacheVullingInfo()
	{
		return String.format("MG:%s,HA:%s,ZI:%s", mammografenInCache(), huisartsenInCache(), zorginstellingeninCache());
	}
}
