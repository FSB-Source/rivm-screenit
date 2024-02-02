package nl.rivm.screenit.huisartsenportaal.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import javax.jms.Message;

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.huisartsenportaal.dto.OvereenkomstDto;
import nl.rivm.screenit.huisartsenportaal.dto.ResetDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingDto;
import nl.rivm.screenit.huisartsenportaal.dto.WoonplaatsDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.service.BetalingService;
import nl.rivm.screenit.huisartsenportaal.service.HuisartsService;
import nl.rivm.screenit.huisartsenportaal.service.LabformulierService;
import nl.rivm.screenit.huisartsenportaal.service.LocatieService;
import nl.rivm.screenit.huisartsenportaal.service.OvereenkomstService;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;
import nl.rivm.screenit.huisartsenportaal.service.VerrichtingenService;
import nl.rivm.screenit.huisartsenportaal.service.WoonplaatsService;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.annotation.JmsListener;
import org.springframework.stereotype.Component;

@Component
public class MessageListener
{

	private static final Logger LOG = LoggerFactory.getLogger(MessageListener.class);

	@Autowired
	private HuisartsService huisartsService;

	@Autowired
	private OvereenkomstService overeenkomstService;

	@Autowired
	private LabformulierService labformulierService;

	@Autowired
	private WoonplaatsService woonplaatsService;

	@Autowired
	private LocatieService locatieService;

	@Autowired
	private VerrichtingenService verrichtingenService;

	@Autowired
	private BetalingService betalingService;

	@Autowired
	private SynchronisatieService synchronisatieService;

	@JmsListener(containerFactory = "jmsListenerContainerFactory", destination = "nl.rivm.screenit.huisartsportaal.${app.environment}")
	private void newMessage(Message message)
	{
		try
		{
			String loginfo = "Synchronisatie bericht " + message.getJMSMessageID();
			if (message instanceof ActiveMQObjectMessage)
			{
				ActiveMQObjectMessage objectMessage = (ActiveMQObjectMessage) message;
				Object object = objectMessage.getObject();

				if (object instanceof HuisartsDto)
				{
					HuisartsDto dto = (HuisartsDto) object;
					LOG.info(loginfo + " voor type huisarts(ha_id: " + dto.getHuisartsportaalId() + ", s_id: " + dto.getScreenitId() + ")");
					huisartsService.updateAndGetHuisarts(dto);
				}

				else if (object instanceof LocatieDto)
				{
					LocatieDto dto = (LocatieDto) object;
					Huisarts huisarts = huisartsService.getHuisartsWith(dto.getHuisartsId());
					LOG.info(loginfo + " voor type locatie(ha_id: " + dto.getHuisartsportaalId() + ", s_id: " + dto.getScreenitId() + ")");
					locatieService.updateAndGetLocatie(huisarts, dto);
					locatieService.nietVerstuurdeLabformulierenVerwijderen(dto);
				}
				else if (object instanceof OvereenkomstDto)
				{
					OvereenkomstDto dto = (OvereenkomstDto) object;
					LOG.info(loginfo + " voor type overeenkomst(ha_id: " + dto.getHuisartsportaalId() + ", s_id: " + dto.getScreenitId() + ")");
					overeenkomstService.saveOrUpdateOvereenkomst((OvereenkomstDto) object);
				}
				else if (object instanceof AanvraagDto)
				{
					AanvraagDto dto = (AanvraagDto) object;
					LOG.info(loginfo + " voor type aanvraag(ha_id: " + dto.getHuisartsportaalId() + ", s_id: " + dto.getScreenitId() + ")");
					labformulierService.saveScreenITAanvraag((AanvraagDto) object);
				}
				else if (object instanceof WoonplaatsDto)
				{
					WoonplaatsDto dto = (WoonplaatsDto) object;
					LOG.info(loginfo + " voor type woonplaats(ha_id: " + dto.getHuisartsportaalId() + ", s_id: " + dto.getScreenitId() + ")");
					woonplaatsService.saveScreenITWoonplaats((WoonplaatsDto) object);
				}
				else if (object instanceof ResetDto)
				{
					ResetDto dto = (ResetDto) object;
					Huisarts huisarts = huisartsService.getHuisartsWith(dto.getHuisarts_id());
					huisarts = huisartsService.wachtwoordVergeten(huisarts);
					synchronisatieService.syncHuisarts(huisarts);
				}
				else if (object instanceof VerrichtingDto)
				{
					VerrichtingDto dto = (VerrichtingDto) object;
					LOG.info(loginfo + " voor type verrichting(ha_id: " + dto.getHuisartsportaalId() + ", s_id: " + dto.getScreenitId() + ")");
					verrichtingenService.saveScreenITVerrichting(dto);
				}

				LOG.info("Bericht succesvol verwerkt! " + message.getJMSMessageID());
			}
		}
		catch (JMSException e)
		{
			LOG.error("Er is een fout opgetreden tijdens het verwerken van het binnengekomen JMS bericht.", e);
		}
	}
}
