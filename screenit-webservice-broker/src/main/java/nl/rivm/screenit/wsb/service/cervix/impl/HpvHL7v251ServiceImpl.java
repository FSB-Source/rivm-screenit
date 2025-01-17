package nl.rivm.screenit.wsb.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.io.IOException;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvBerichtWrapper;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.repository.cervix.BmhkLaboratoriumRepository;
import nl.rivm.screenit.repository.cervix.CervixHpvBerichtRepository;
import nl.rivm.screenit.service.cervix.CervixFactory;
import nl.rivm.screenit.specification.cervix.CervixBMHKLaboratoriumSpecification;
import nl.rivm.screenit.specification.cervix.CervixHpvBerichtSpecification;
import nl.rivm.screenit.wsb.service.BaseHL7v2Service;
import nl.rivm.screenit.wsb.service.cervix.HpvHL7v251Service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.UncategorizedJmsException;
import org.springframework.stereotype.Service;

import ca.uhn.hl7v2.AcknowledgmentCode;
import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.app.ApplicationException;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;

@Service
@Slf4j
public class HpvHL7v251ServiceImpl extends BaseHL7v2Service<OUL_R22> implements HpvHL7v251Service
{
	@Autowired
	private BmhkLaboratoriumRepository bmhkLaboratoriumRepository;

	@Autowired
	private CervixHpvBerichtRepository hpvBerichtRepository;

	@Autowired
	private CervixFactory factory;

	@Override
	public Message processTypedMessage(OUL_R22 message) throws ApplicationException, HL7Exception
	{
		var screenitMessage = new CervixHpvBerichtWrapper(message);
		Long labId = null;
		try
		{

			if (screenitMessage.isValid())
			{
				verwerkBericht(screenitMessage);

				if (screenitMessage.getResultaten().isEmpty())
				{
					var foutmelding = "Dit bericht bevat alleen controlemonsters. Bericht-ID: "
						+ screenitMessage.getMessageId() + " Laboratorium:" + screenitMessage.getLabnaam() + " ZInstrumentName: " + screenitMessage.getInstrumentId();
					saveLogGebeurtenis(LogGebeurtenis.CERVIX_HPV_BERICHT_REJECTED, foutmelding);
				}

				return message.generateACK();
			}
			else
			{
				var foutmelding = "Dit bericht voldoet niet aan de juiste syntax, er ontbreken velden in het bericht en/of velden zijn niet goed gevuld. Bericht-ID: "
					+ screenitMessage.getMessageId() + " Laboratorium: " + screenitMessage.getLabnaam() + " ZInstrumentName: " + screenitMessage.getInstrumentId();
				saveLogGebeurtenis(LogGebeurtenis.CERVIX_HPV_BERICHT_REJECTED, foutmelding);
				throw new HL7Exception(foutmelding);
			}
		}
		catch (HL7Exception e)
		{
			try
			{
				return message.generateACK(AcknowledgmentCode.AR, e);
			}
			catch (IOException e1)
			{
				LOG.error("Er kan geen ACK reject worden gemaakt", e);
			}
		}
		catch (Exception e)
		{
			try
			{
				LOG.error("Er is een fout opgetreden met de verwerking van het HL7 bericht. AE send", e);
				return message.generateACK(AcknowledgmentCode.AE, new HL7Exception("Er is een fout opgetreden met de verwerking van het HL7 bericht."));
			}
			catch (IOException e1)
			{
				LOG.error("Er is gewoon helemaal niks meer mogelijk!", e);
			}
		}
		finally
		{
			try
			{

				verwerkBerichtService.queueHPVBericht(labId);
			}
			catch (UncategorizedJmsException e)
			{
				LOG.error("Er is een probleem met ActiveMQ.", e);
			}

		}
		return null;
	}

	private void verwerkBericht(CervixHpvBerichtWrapper berichtWrapper) throws HL7Exception
	{
		var laboratorium = getBmhkLaboratorium(berichtWrapper);

		if (hpvBerichtRepository.exists(CervixHpvBerichtSpecification.heeftMessageId(berichtWrapper.getMessageId())))
		{
			LOG.warn("Bericht al eerder binnengekomen voor lab: {}", laboratorium.getNaam());
			var melding = "Bericht (messageID: " + berichtWrapper.getMessageId() + ") al eerder binnengekomen voor lab: " + laboratorium.getNaam();
			logging(LogGebeurtenis.CERVIX_HPV_BERICHT_BINNENGEKOMEN, Level.WARNING, laboratorium, melding, Bevolkingsonderzoek.CERVIX);
			return;
		}

		logging(LogGebeurtenis.CERVIX_HPV_BERICHT_BINNENGEKOMEN, Level.INFO, laboratorium,
			"Bericht (messageID: " + berichtWrapper.getMessageId() + ") binnengekomen voor lab: " + laboratorium.getNaam(), Bevolkingsonderzoek.CERVIX);
		factory.maakHpvBericht(laboratorium, berichtWrapper.getInstrumentId(), berichtWrapper.getMessage().toString(), berichtWrapper.getMessageId());
	}

	private BMHKLaboratorium getBmhkLaboratorium(CervixHpvBerichtWrapper message) throws HL7Exception
	{
		var instrumentId = message.getInstrumentId();
		var laboratorium = bmhkLaboratoriumRepository.findOne(CervixBMHKLaboratoriumSpecification.heeftZInstrumentNames(instrumentId))
			.orElse(null);
		if (laboratorium != null)
		{
			return laboratorium;
		}
		logging(LogGebeurtenis.CERVIX_HPV_BERICHT_BINNENGEKOMEN, Level.ERROR, null,
			"Bericht (messageID: " + message.getMessageId() + ") binnengekomen, geen lab gevonden met ZInstrumentName: " + instrumentId, Bevolkingsonderzoek.CERVIX);
		throw new HL7Exception("Geen lab gevonden met dit ZInstrumentName");
	}

	private void saveLogGebeurtenis(LogGebeurtenis logGebeurtenis, String message)
	{
		try
		{
			logService.logGebeurtenis(logGebeurtenis, new LogEvent(message), Bevolkingsonderzoek.CERVIX);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
	}
}
