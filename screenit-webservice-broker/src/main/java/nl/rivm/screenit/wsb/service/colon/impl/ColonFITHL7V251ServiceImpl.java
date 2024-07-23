package nl.rivm.screenit.wsb.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.io.IOException;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.berichten.ColonIFobtHL7BerichtWrapper;
import nl.rivm.screenit.model.colon.berichten.ColonIFobtUitslagBericht;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.colon.ColonFITLaboratoriumRepository;
import nl.rivm.screenit.repository.colon.ColonFITUitslagBerichtRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.wsb.service.BaseHL7v2Service;
import nl.rivm.screenit.wsb.service.colon.ColonFITHL7v251Service;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.UncategorizedJmsException;
import org.springframework.stereotype.Service;

import ca.uhn.hl7v2.AcknowledgmentCode;
import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;

@Service
@Slf4j
public class ColonFITHL7V251ServiceImpl extends BaseHL7v2Service<OUL_R22> implements ColonFITHL7v251Service
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private ColonFITLaboratoriumRepository fitLaboratoriumRepository;

	@Autowired
	private ColonFITUitslagBerichtRepository fitUitslagBerichtRepository;

	@Override
	public Message processTypedMessage(OUL_R22 message)
	{
		ColonIFobtHL7BerichtWrapper bericht = null;
		try
		{
			bericht = new ColonIFobtHL7BerichtWrapper(message);
			verwerkBericht(bericht);
			return message.generateACK();
		}
		catch (NullPointerException | HL7Exception ex)
		{
			return generateApplicationError(ex, message);
		}
		catch (Exception e)
		{
			return generateApplicationReject(e, message);
		}
		finally
		{
			if (bericht != null)
			{
				try
				{
					verwerkBerichtService.queueIFobtBericht(bericht.getMessageId());
				}
				catch (UncategorizedJmsException e)
				{
					LOG.error("Er is een probleem met ActiveMQ.", e);
				}
			}
		}
	}

	private void verwerkBericht(ColonIFobtHL7BerichtWrapper berichtWrapper) throws HL7Exception
	{
		var laboratorium = getLaboratorium(berichtWrapper);

		if (fitUitslagBerichtRepository.existsByMessageId(berichtWrapper.getMessageId()))
		{
			var melding = "FIT HL7 Bericht (messageID: " + berichtWrapper.getMessageId() + ") al eerder binnengekomen voor lab: " + laboratorium.getNaam();
			logging(LogGebeurtenis.COLON_IFOBT_BERICHT_BINNENGEKOMEN, Level.WARNING, laboratorium, melding, Bevolkingsonderzoek.COLON);
			return;
		}

		logging(LogGebeurtenis.COLON_IFOBT_BERICHT_BINNENGEKOMEN, Level.INFO, laboratorium,
			"Bericht (messageID: " + berichtWrapper.getMessageId() + ") binnengekomen voor lab: " + laboratorium.getNaam(), Bevolkingsonderzoek.COLON);

		var uitslagBericht = new ColonIFobtUitslagBericht();
		uitslagBericht.setMessageId(berichtWrapper.getMessageId());
		uitslagBericht.setStatus(BerichtStatus.NIEUW);
		uitslagBericht.setLaboratorium(laboratorium);
		uitslagBericht.setStatusDatum(dateSupplier.getDate());
		uitslagBericht.setOntvangen(dateSupplier.getDate());
		uitslagBericht.setHl7Bericht(berichtWrapper.getMessage().toString());
		hibernateService.saveOrUpdate(uitslagBericht);
	}

	private IFobtLaboratorium getLaboratorium(ColonIFobtHL7BerichtWrapper message) throws HL7Exception
	{
		var labId = message.getLabId();
		var laboratorium = fitLaboratoriumRepository.findByLabId(labId);
		if (laboratorium != null)
		{
			return laboratorium;
		}
		throw new HL7Exception("FIT HL7 Bericht (messageID: " + message.getMessageId() + ") binnengekomen, geen lab gevonden met ID: " + labId);
	}

	private Message generateApplicationError(Exception ex, Message message)
	{
		var foutmelding = ex.getMessage();

		if (StringUtils.isBlank(foutmelding))
		{
			foutmelding = "Er is een (syntax)fout gevonden bij de verwerking van het FIT HL7 bericht. Het bericht zal niet verder worden verwerkt.";
		}
		LOG.error(foutmelding, ex);
		try
		{
			logging(LogGebeurtenis.COLON_IFOBT_BERICHT_ERROR, Level.ERROR, null, foutmelding, Bevolkingsonderzoek.COLON);
			return message.generateACK(AcknowledgmentCode.AE, new HL7Exception("Er is een fout opgetreden met de verwerking van het HL7 bericht."));
		}
		catch (HL7Exception | IOException e)
		{
			LOG.error("Er is gewoon helemaal niks meer mogelijk!", e);
		}
		return null;
	}

	private Message generateApplicationReject(Exception e, Message message)
	{
		try
		{
			var foutmelding = "Er is een onbekende fout opgetreden in de applicatie.";
			LOG.error(foutmelding, e);
			return message.generateACK(AcknowledgmentCode.AR, new HL7Exception(foutmelding));
		}
		catch (HL7Exception | IOException e1)
		{
			LOG.error("Er is gewoon helemaal niks meer mogelijk!", e1);
		}
		return null;
	}

}
