package nl.rivm.screenit.mamma.se.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.jms.JMSException;
import javax.jms.Session;

import lombok.AllArgsConstructor;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.se.service.DaglijstService;
import nl.rivm.screenit.mamma.se.websocket.socket.SeProxyWebsocket;
import nl.rivm.screenit.websocket.WebsocketBerichtType;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.springframework.jms.listener.SessionAwareMessageListener;
import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class SeRestMessageListener implements SessionAwareMessageListener<ActiveMQObjectMessage>
{
	private final DaglijstService daglijstService;

	private final SeProxyWebsocket seProxyWebsocket;

	@Override
	public void onMessage(ActiveMQObjectMessage message, @NonNull Session session)
	{
		try
		{
			Object object = message.getObject();

			if (object instanceof String)
			{
				String bericht = (String) object;
				if (bericht.startsWith("SE"))
				{
					daglijstService.verstuurUpdate(bericht);
				}
				else if (bericht.equals(WebsocketBerichtType.VERSTUUR_STATUS.name()))
				{
					seProxyWebsocket.sendVerstuurStatusVerzoekNaarIedereSe();
				}
				else
				{
					seProxyWebsocket.sendDbCleanupNaarIedereSe();
				}
			}
			else
			{
				LOG.error("Onbekend bericht ontvangen via ActiveMQ op SE-REST-BK, JMS-ID: " + message.getJMSMessageID());
			}

		}
		catch (JMSException e)
		{
			LOG.error("Er is een fout opgetreden tijdens het verwerken van het binnengekomen JMS bericht.", e);
		}
	}

}
