package nl.rivm.screenit.wsb.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.io.IOException;

import nl.rivm.screenit.dao.colon.IFOBTResultDao;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.berichten.ColonIFobtHL7BerichtWrapper;
import nl.rivm.screenit.model.colon.berichten.ColonIFobtUitslagBericht;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.wsb.service.BaseHL7v2Service;
import nl.rivm.screenit.wsb.service.colon.IFobtHL7v251Service;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.UncategorizedJmsException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.AcknowledgmentCode;
import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class IFobtHL7V251ServiceImpl extends BaseHL7v2Service<OUL_R22> implements IFobtHL7v251Service
{
	private static final Logger LOG = LoggerFactory.getLogger(IFobtHL7V251ServiceImpl.class);

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private IFOBTResultDao ifobtResultDao;

	@Override
	public Message processTypedMessage(OUL_R22 message)
	{
		ColonIFobtHL7BerichtWrapper ifobtBericht = null;
		try
		{
			ifobtBericht = new ColonIFobtHL7BerichtWrapper(message);
			LOG.info("Incoming message: hl7v251(" + ifobtBericht.getMessageId() + ")");

			verwerkBericht(ifobtBericht);

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
			if (ifobtBericht != null)
			{
				try
				{
					verwerkBerichtService.queueIFobtBericht(ifobtBericht.getMessageId());
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
		IFobtLaboratorium laboratorium = getIFobtLaboratorium(berichtWrapper);

		if (ifobtResultDao.isIFobtBerichtAlOntvangen(berichtWrapper.getMessageId()))
		{
			String melding = "FIT HL7 Bericht (messageID: " + berichtWrapper.getMessageId() + ") al eerder binnengekomen voor lab: " + laboratorium.getNaam();
			logging(LogGebeurtenis.COLON_IFOBT_BERICHT_BINNENGEKOMEN, Level.WARNING, laboratorium, melding, Bevolkingsonderzoek.COLON);
			return;
		}

		logging(LogGebeurtenis.COLON_IFOBT_BERICHT_BINNENGEKOMEN, Level.INFO, laboratorium,
			"Bericht (messageID: " + berichtWrapper.getMessageId() + ") binnengekomen voor lab: " + laboratorium.getNaam(), Bevolkingsonderzoek.COLON);

		ColonIFobtUitslagBericht iFobtUitslagBericht = new ColonIFobtUitslagBericht();
		iFobtUitslagBericht.setMessageId(berichtWrapper.getMessageId());
		iFobtUitslagBericht.setStatus(BerichtStatus.NIEUW);
		iFobtUitslagBericht.setLaboratorium(laboratorium);
		iFobtUitslagBericht.setStatusDatum(dateSupplier.getDate());
		iFobtUitslagBericht.setOntvangen(dateSupplier.getDate());
		iFobtUitslagBericht.setHl7Bericht(berichtWrapper.getMessage().toString());
		hibernateService.saveOrUpdate(iFobtUitslagBericht);
	}

	private IFobtLaboratorium getIFobtLaboratorium(ColonIFobtHL7BerichtWrapper message) throws HL7Exception
	{
		String labId = message.getLabId();
		IFobtLaboratorium laboratorium = ifobtResultDao.getIFobtLaboratorium(labId);
		if (laboratorium != null)
		{
			return laboratorium;
		}
		throw new HL7Exception("FIT HL7 Bericht (messageID: " + message.getMessageId() + ") binnengekomen, geen lab gevonden met ID: " + labId);
	}

	private Message generateApplicationError(Exception ex, Message message)
	{
		String foutmelding = ex.getMessage();

		if (StringUtils.isBlank(foutmelding))
		{
			foutmelding = "Er is een (syntax)fout gevonden bij de verwerking van het FIT HL7 bericht. Het bericht zal niet verder worden verwerkt.";
		}
		LOG.error(foutmelding, ex);
		try
		{
			logging(LogGebeurtenis.COLON_IFOBT_BERICHT_ERROR, Level.ERROR, null, foutmelding, Bevolkingsonderzoek.COLON);
			Message response = message.generateACK(AcknowledgmentCode.AE, new HL7Exception("Er is een fout opgetreden met de verwerking van het HL7 bericht."));
			return response;
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
			String foutmelding = "Er is een onbekende fout opgetreden in de applicatie.";
			LOG.error(foutmelding, e);
			Message responseReject = message.generateACK(AcknowledgmentCode.AR, new HL7Exception(foutmelding));
			return responseReject;
		}
		catch (HL7Exception | IOException e1)
		{
			LOG.error("Er is gewoon helemaal niks meer mogelijk!", e1);
		}
		return null;
	}

}
