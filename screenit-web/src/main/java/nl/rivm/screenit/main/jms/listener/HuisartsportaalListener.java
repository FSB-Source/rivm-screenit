package nl.rivm.screenit.main.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsSyncService;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.listener.SessionAwareMessageListener;

public class HuisartsportaalListener implements SessionAwareMessageListener<ActiveMQObjectMessage>
{

	private static final Logger LOG = LoggerFactory.getLogger(HuisartsportaalListener.class);

	@Autowired
	private CervixHuisartsSyncService cervixHuisartsSyncService;

	@Override
	public void onMessage(ActiveMQObjectMessage objectMessage, Session session)
	{
		try
		{
			String loginfo = "Synchronisatie bericht " + objectMessage.getJMSMessageID();
			Object object = objectMessage.getObject();

			if (object instanceof AanvraagDto)
			{
				AanvraagDto aanvraagDto = (AanvraagDto) object;
				LOG.info(loginfo + " voor type aanvraag(ha_id: " + aanvraagDto.getHuisartsportaalId() + ", s_id: " + aanvraagDto.getScreenitId() + ")");
				cervixHuisartsSyncService.setLabformulierAanvraag(aanvraagDto);
			}
			else if (object instanceof LocatieDto)
			{
				LocatieDto locatieDto = (LocatieDto) object;
				LOG.info(loginfo + " voor type aanvraag(ha_id: " + locatieDto.getHuisartsportaalId() + ", s_id: " + locatieDto.getScreenitId() + ")");
				cervixHuisartsSyncService.updateLocatie(locatieDto);
			}
			else if (object instanceof HuisartsDto)
			{
				HuisartsDto huisartsDto = (HuisartsDto) object;
				LOG.info(loginfo + " voor type aanvraag(ha_id: " + huisartsDto.getHuisartsportaalId() + ", s_id: " + huisartsDto.getScreenitId() + ")");
				cervixHuisartsSyncService.updateHuisarts(huisartsDto);
			}
			LOG.info("Bericht succesvol verwerkt! " + objectMessage.getJMSMessageID());
		}
		catch (JMSException e)
		{
			LOG.error("Er is een fout opgetreden tijdens het verwerken van het binnengekomen JMS bericht.", e);
		}
	}
}
