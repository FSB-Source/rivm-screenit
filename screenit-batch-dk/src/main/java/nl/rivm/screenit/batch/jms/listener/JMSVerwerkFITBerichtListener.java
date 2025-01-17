package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.ColonFITHL7BerichtInlezenService;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.springframework.jms.listener.SessionAwareMessageListener;
import org.springframework.stereotype.Component;

@Component
@Slf4j
@AllArgsConstructor
public class JMSVerwerkFITBerichtListener implements SessionAwareMessageListener<ActiveMQObjectMessage>
{

	private final ColonFITHL7BerichtInlezenService fitHL7BerichtInlezenService;

	@Override
	public void onMessage(ActiveMQObjectMessage activeMQObjectMessage, Session session) throws JMSException
	{
		var berichten = fitHL7BerichtInlezenService.getAlleNietVerwerkteFitBerichten();
		for (var bericht : berichten)
		{
			try
			{
				fitHL7BerichtInlezenService.verwerkOntvangenFitBericht(bericht);
			}
			catch (Exception e)
			{
				LOG.error("Er is een probleem opgetreden, met het verwerken van de hpv data", e);
				fitHL7BerichtInlezenService.logError(bericht, e.getMessage(), null);
			}
		}

	}
}
