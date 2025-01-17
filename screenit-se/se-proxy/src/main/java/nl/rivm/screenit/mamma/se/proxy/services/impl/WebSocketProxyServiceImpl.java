package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.format.DateTimeFormatter;
import java.util.Base64;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.proxy.SeProxyApplication;
import nl.rivm.screenit.mamma.se.proxy.model.WebsocketBerichtType;
import nl.rivm.screenit.mamma.se.proxy.services.WebSocketProxyService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.messaging.handler.annotation.SendTo;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
@Slf4j
public class WebSocketProxyServiceImpl implements WebSocketProxyService
{
	@Autowired
	private SimpMessagingTemplate simp;

	@Value("${MAMMOGRAAF_STUB_URL}")
	private String mammograafStubUrl;

	@SendTo("/transactionReceive")
	@Override
	public void broadcast(String transactionJSON)
	{
		if (transactionJSON.equals("ONLINE") || transactionJSON.equals("OFFLINE"))
		{
			LOG.info("Online status broadcast: " + transactionJSON);
		}
		simp.convertAndSend("/transactionReceive", transactionJSON);
	}

	@Override
	public void broadCastTijdUpdateNaarWerkstations(String logPrefix)
	{
		if (SeProxyApplication.getEnvironmentInfo().getEnvironment().equals("Test"))
		{
			String offset = DateUtil.getOffset().toString();
			LOG.info("{}: Broadcast tijdupdate naar werkstations voor offset: {} (nieuwe tijd: {})", logPrefix, offset, DateUtil.getCurrentDateTime());
			broadcast(WebsocketBerichtType.TIJD_UPDATE.name() + "###" + offset);

			HttpHeaders headers = new HttpHeaders();
			headers.add("Content-Type", MediaType.APPLICATION_JSON_VALUE);
			var auth = "beheer:mammograafStub!";
			var encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
			var authHeader = "Basic " + encodedAuth;
			headers.add("Authorization", authHeader);

			var restTemplate = new RestTemplate();
			var request = new HttpEntity<>(headers);
			try
			{
				restTemplate.postForObject(mammograafStubUrl + "/wijzigDatumTijd/" + DateUtil.getCurrentDateTime().format(DateTimeFormatter.ISO_DATE_TIME), request, String.class);
			}
			catch (Exception e)
			{
				LOG.error("Fout bij wijzigen van datum/tijd in mammograaf stub {}: {}", mammograafStubUrl, e.getMessage());
			}
		}
	}
}
