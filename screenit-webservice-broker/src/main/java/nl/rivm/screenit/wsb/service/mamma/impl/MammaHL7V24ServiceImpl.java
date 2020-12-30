package nl.rivm.screenit.wsb.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.mamma.MammaIMSDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.MammaHl7v24BerichtLogEvent;
import nl.rivm.screenit.model.mamma.berichten.MammaHL7OntvangenBerichtWrapper;
import nl.rivm.screenit.model.mamma.berichten.MammaIMSBericht;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.wsb.service.BaseHL7v2Service;
import nl.rivm.screenit.wsb.service.mamma.MammaHL7v24Service;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

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
import ca.uhn.hl7v2.model.v24.message.ORM_O01;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public abstract class MammaHL7V24ServiceImpl extends BaseHL7v2Service<ORM_O01> implements MammaHL7v24Service
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaHL7V24ServiceImpl.class);

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaIMSDao mammaIMSDao;

	@Autowired
	private ClientService clientService;

	@Autowired
	private LogService logService;

	@Autowired
	private BezwaarService bezwaarService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public Message processTypedMessage(ORM_O01 message)
	{
		MammaHL7OntvangenBerichtWrapper berichtWrapper = null;
		try
		{
			berichtWrapper = new MammaHL7OntvangenBerichtWrapper(message);
			verwerkBericht(berichtWrapper);
			return message.generateACK();
		}
		catch (Exception e)
		{
			return generateApplicationReject(e, message);
		}
		finally
		{
			if (berichtWrapper != null)
			{
				try
				{
					verwerkBerichtService.queueMammaInkomendIMSBericht(berichtWrapper.getMessageId());
				}
				catch (UncategorizedJmsException e)
				{
					LOG.error("Er is een probleem met ActiveMQ.", e);
				}
			}
		}
	}

	private Message generateApplicationReject(Exception exception, Message message)
	{
		try
		{
			if (!(exception instanceof HL7Exception))
			{
				LOG.error("Er is een onbekende fout opgetreden in de applicatie.", exception);
			}
			MammaHl7v24BerichtLogEvent logEvent = new MammaHl7v24BerichtLogEvent();
			logEvent.setHl7MessageStructure(message.toString());
			logEvent.setMelding(exception.getMessage());
			logEvent.setLevel(Level.ERROR);

			logService.logGebeurtenis(LogGebeurtenis.MAMMA_HL7_BERICHT_ONTVANGEN_MISLUKT,
				logEvent,
				Bevolkingsonderzoek.MAMMA);

			Message responseReject = message.generateACK(AcknowledgmentCode.AR, new HL7Exception(exception));
			return responseReject;
		}
		catch (Exception e)
		{
			LOG.error("Er is iets fout gegaan bij het maken van het reject bericht", e);
		}
		return null;
	}

	private void verwerkBericht(MammaHL7OntvangenBerichtWrapper berichtWrapper) throws HL7Exception
	{
		MammaIMSBericht beeldenBeschikbaarBericht = new MammaIMSBericht();
		beeldenBeschikbaarBericht.setAccessionNumber(berichtWrapper.getAccessionNumber());
		beeldenBeschikbaarBericht.setBsn(berichtWrapper.getBsn());
		beeldenBeschikbaarBericht.setHl7Bericht(berichtWrapper.getMessage().toString());
		beeldenBeschikbaarBericht.setMessageId(berichtWrapper.getMessageId());
		beeldenBeschikbaarBericht.setBerichtStatus(BerichtStatus.NIEUW);
		beeldenBeschikbaarBericht.setOrmStatus(berichtWrapper.getStatus());
		beeldenBeschikbaarBericht.setOntvangstDatum(dateSupplier.getDate());

		Client client = clientService.getClientByBsn(berichtWrapper.getBsn());

		if (!getAcceptedOrmBerichtStatussen().contains(berichtWrapper.getStatus()))
		{
			throw new HL7Exception("Ontvangen IMS bericht heeft niet de verwachte status maar: " + berichtWrapper.getStatus().name());
		}

		if (!mammaIMSDao.isBerichtAlOntvangen(berichtWrapper.getMessageId()))
		{
			if (isValideBerichtVoorClient(client, berichtWrapper))
			{
				hibernateService.saveOrUpdate(beeldenBeschikbaarBericht);
			}
			else
			{
				String melding = String.format("Ontvangen IMS bericht (%s) kon niet gekoppeld worden aan BSN (%s) en/of accession number (%s).",
					berichtWrapper.getMessageId(), berichtWrapper.getBsn(), berichtWrapper.getAccessionNumber());
				throw new HL7Exception(melding);
			}
		}
		else
		{
			MammaHl7v24BerichtLogEvent logEvent = new MammaHl7v24BerichtLogEvent();
			logEvent.setClient(client);
			logEvent.setHl7MessageStructure(berichtWrapper.getMessage().toString());
			logEvent.setMelding("IMS HL7 Bericht (messageID: " + berichtWrapper.getMessageId() + ") binnengekomen, bericht bestaat al");
			logEvent.setLevel(Level.WARNING);
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_HL7_BERICHT_AL_ONTVANGEN, logEvent, null, client, Bevolkingsonderzoek.MAMMA);
		}
	}

	private boolean isVerwijderdMetBezwaar(Client client, MammaHL7v24ORMBerichtStatus status)
	{
		return (status.equals(MammaHL7v24ORMBerichtStatus.DELETED) || status.equals(MammaHL7v24ORMBerichtStatus.ERROR)) && bezwaarService.heeftBezwaarInAfgelopenAantalDagen(
			client,
			BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER,
			Bevolkingsonderzoek.MAMMA,
			preferenceService.getInteger(PreferenceKey.ILM_BEZWAARTERMIJN_BEELDEN_VERWIJDERD.name()));
	}

	abstract List<MammaHL7v24ORMBerichtStatus> getAcceptedOrmBerichtStatussen();

	private boolean isValideBerichtVoorClient(Client client, MammaHL7OntvangenBerichtWrapper berichtWrapper)
	{
		if (client == null)
		{
			return false;
		}
		return isMammografieAccessionNumberVanClient(berichtWrapper.getAccessionNumber(), client) || isAccessionNumberVanUploadPoging(berichtWrapper.getAccessionNumber(),
			client) || isVerwijderdMetBezwaar(client, berichtWrapper.getStatus());
	}

	private boolean isMammografieAccessionNumberVanClient(Long accessionNumber, Client client)
	{
		return client.getMammaDossier().getScreeningRondes().stream().anyMatch(screeningRonde -> screeningRonde.getUitnodigingsNr().equals(accessionNumber));
	}

	private boolean isAccessionNumberVanUploadPoging(Long accessionNumber, Client client)
	{
		return client.getMammaDossier().getScreeningRondes().stream().flatMap(screeningRonde -> screeningRonde.getUploadBeeldenVerzoeken().stream())
			.flatMap(uploadBeeldenVerzoek -> uploadBeeldenVerzoek.getUploadPogingen().stream())
			.anyMatch(uploadBeeldenPoging -> uploadBeeldenPoging.getAccessionNumber() != null && uploadBeeldenPoging.getAccessionNumber().equals(accessionNumber));
	}

}
