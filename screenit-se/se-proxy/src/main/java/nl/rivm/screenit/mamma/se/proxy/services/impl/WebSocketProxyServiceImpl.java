package nl.rivm.screenit.mamma.se.proxy.services.impl;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.proxy.SeProxyApplication;
import nl.rivm.screenit.mamma.se.proxy.model.WebsocketBerichtType;
import nl.rivm.screenit.mamma.se.proxy.services.WebSocketProxyService;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.handler.annotation.SendTo;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

@Service
public class WebSocketProxyServiceImpl implements WebSocketProxyService
{
	private static final Logger LOG = LoggerFactory.getLogger(WebSocketProxyService.class);

	@Autowired
	private SimpMessagingTemplate simp;

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
		}
	}
}
