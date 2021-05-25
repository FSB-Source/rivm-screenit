package nl.rivm.screenit.mamma.se.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.service.DaglijstService;
import nl.rivm.screenit.mamma.se.websocket.socket.SeProxyWebsocket;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.listener.SessionAwareMessageListener;

public class SeRestMessageListener implements SessionAwareMessageListener<ActiveMQObjectMessage>
{
	private static final Logger LOG = LoggerFactory.getLogger(SeRestMessageListener.class);

	@Autowired
	private DaglijstService daglijstService;

	@Autowired
	private SeProxyWebsocket seProxyWebsocket;

	@Override
	public void onMessage(ActiveMQObjectMessage message, Session session) throws JMSException
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
