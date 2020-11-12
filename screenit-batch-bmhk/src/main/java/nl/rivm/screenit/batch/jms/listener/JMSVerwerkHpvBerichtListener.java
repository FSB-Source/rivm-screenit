package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import javax.jms.JMSException;
import javax.jms.Session;

import nl.rivm.screenit.batch.service.CervixVerwerkHpvBerichtService;
import nl.rivm.screenit.model.cervix.CervixHpvBericht;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.listener.SessionAwareMessageListener;

public class JMSVerwerkHpvBerichtListener implements SessionAwareMessageListener<ActiveMQObjectMessage>
{
	
	private static final Logger LOG = LoggerFactory.getLogger(JMSVerwerkHpvBerichtListener.class);

	@Autowired
	private CervixVerwerkHpvBerichtService verwerkHpvBerichtService;

	@Override
	public void onMessage(ActiveMQObjectMessage message, Session session) throws JMSException
	{

		List<CervixHpvBericht> berichten = verwerkHpvBerichtService.getAlleNietVerwerkteHpvBerichten();
		for (CervixHpvBericht bericht : berichten)
		{
			try
			{
				verwerkHpvBerichtService.verwerkOntvangenHpvBericht(bericht.getId());
			}
			catch (Exception e)
			{
				LOG.error("Er is een probleem opgetreden, met het verwerken van de hpv data", e);
				verwerkHpvBerichtService.logError(bericht.getId(), e.getMessage());
			}
		}
	}

}
