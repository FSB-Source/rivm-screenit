package nl.rivm.screenit.wsb.interceptors;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.apache.cxf.binding.soap.interceptor.EndpointSelectionInterceptor;
import org.apache.cxf.common.util.UrlUtils;
import org.apache.cxf.endpoint.Endpoint;
import org.apache.cxf.frontend.WSDLGetInterceptor;
import org.apache.cxf.frontend.WSDLGetUtils;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.message.Message;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.apache.cxf.service.model.EndpointInfo;
import org.apache.cxf.transport.http.AbstractHTTPDestination;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LocationUriInterceptor extends AbstractPhaseInterceptor<Message>
{
	public static final LocationUriInterceptor INSTANCE = new LocationUriInterceptor();

	private static final Logger LOG = LoggerFactory.getLogger(LocationUriInterceptor.class);

	private String wsContext;

	public LocationUriInterceptor()
	{
		super(Phase.READ);
		getAfter().add(EndpointSelectionInterceptor.class.getName());
		getBefore().add(WSDLGetInterceptor.class.getName());
	}

	@Override
	public void handleMessage(Message message) throws Fault
	{
		String method = (String) message.get(Message.HTTP_REQUEST_METHOD);
		String query = (String) message.get(Message.QUERY_STRING);

		if (!"GET".equals(method) || StringUtils.isEmpty(query))
		{
			return;
		}
		Map<String, String> map = UrlUtils.parseQueryString(query);
		if (map.containsKey("wsdl") || map.containsKey("xsd"))
		{
			String baseUri = "<Unkown>";
			try
			{
				HttpServletRequest request = (HttpServletRequest) message.get(AbstractHTTPDestination.HTTP_REQUEST);
				String host = request.getHeader("X-Forwarded-Host");
				if (StringUtils.isNotBlank(host) && !"unknown".equalsIgnoreCase(host))
				{
					String[] splittedHost = host.split(",");
					host = splittedHost[0].trim();
				}
				else
				{
					host = request.getHeader("Host");
					if (StringUtils.isNotBlank(host) && !"unknown".equalsIgnoreCase(host))
					{
						String[] splittedHost = host.split(",");
						host = splittedHost[0].trim();
					}
				}
				String proto = request.getHeader("X-Forwarded-Proto");
				if (StringUtils.isBlank(proto) || "unknown".equalsIgnoreCase(proto))
				{
					proto = "http";
				}
				baseUri = proto + "://" + host + "/" + wsContext;
			}
			catch (Exception e)
			{
				LOG.error("request niet succesvol gestuurd?", e.getMessage());
			}

			Endpoint endpoint = message.getExchange().getEndpoint();

			synchronized (endpoint)
			{
				EndpointInfo endpointInfo = endpoint.getEndpointInfo();
				LOG.info("Publishing endpoint URL is set to " + baseUri);
				endpointInfo.setProperty(WSDLGetUtils.PUBLISHED_ENDPOINT_URL, baseUri);
			}
		}
	}

	public void setWsContext(String wsContext)
	{
		this.wsContext = wsContext;
	}
}
