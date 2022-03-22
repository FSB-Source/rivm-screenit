package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.Serializable;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.jms.Destination;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.MammaAbstractHL7v24OrmBerichtTriggerDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24AdtBerichtTriggerDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetClientDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto;
import nl.rivm.screenit.dto.mamma.se.MammaKwaliteitsopnameDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.gba.GbaMutatie;
import nl.rivm.screenit.model.helper.ActiveMQHelper;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaHL7v24Message;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenPoging;
import nl.rivm.screenit.model.mamma.enums.MammaHL7ADTBerichtType;
import nl.rivm.screenit.model.mamma.enums.MammaHL7BerichtType;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.activemq.command.ActiveMQMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service
public class BerichtToBatchServiceImpl implements BerichtToBatchService
{

	private static final Logger LOG = LoggerFactory.getLogger(BerichtToBatchServiceImpl.class);

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private JmsTemplate jmsTemplate;

	@Autowired
	@Qualifier("verwerkColonCdaBerichtDestination")
	private Destination colonCdaDestination;

	@Autowired
	@Qualifier("verwerkCervixCdaBerichtDestination")
	private Destination cervixCdaDestination;

	@Autowired
	@Qualifier("verwerkMammaCdaBerichtDestination")
	private Destination mammaCdaDestination;

	@Autowired
	@Qualifier("verwerkHpvBerichtDestination")
	private Destination hpvDestination;

	@Autowired
	@Qualifier("verwerkIFobtBerichtDestination")
	private Destination ifobtDestination;

	@Autowired
	@Qualifier("verwerkMammaIMSBerichtDestination")
	private Destination mammaDestination;

	@Autowired
	@Qualifier("verzamelOnderzoekDataBerichtDestination")
	private Destination mammaVerzamelOnderzoekDataBerichtDestination;

	@Autowired
	@Qualifier("uploadBeeldenVerzoekBerichtDestination")
	private Destination mammaUploadBeeldenVerzoekBerichtDestination;

	@Autowired
	private HibernateService hibernateService;

	private final ObjectMapper objectMapper = new ObjectMapper();

	private final String bsnMarkerPattern = "(\\|mammaClientBSNGewijzigd:([0-9]+),([0-9]+)\\|)";

	private final Pattern bsnGewijzigdMarkerPattern = Pattern.compile(bsnMarkerPattern);

	@Override
	public void queueHPVBericht(Long labId)
	{
		LOG.debug("Sending ActiveMq message to batch BMHK");
		queueBericht(hpvDestination, labId);
	}

	@Override
	public void queueIFobtBericht(String berichtId)
	{
		LOG.debug("Sending ActiveMq message to batch DK");
		queueBericht(ifobtDestination, berichtId);
	}

	@Override
	public void queueMammaVerzamelOnderzoeksDataBericht()
	{
		LOG.debug("Sending ActiveMq message to batch BK");
		queueTextBericht(mammaVerzamelOnderzoekDataBerichtDestination, "");
	}

	@Override
	public void queueCDABericht(Bevolkingsonderzoek bvo)
	{
		LOG.debug("Sending ActiveMq message to batch " + bvo.getAfkorting());
		switch (bvo)
		{
		case COLON:
			queueBericht(colonCdaDestination);
			break;
		case CERVIX:
			queueBericht(cervixCdaDestination);
			break;
		case MAMMA:
			queueBericht(mammaCdaDestination);
			break;
		}
	}

	@Override
	public void queueMammaUploadBeeldenVerzoekBericht()
	{
		LOG.debug("Sending uploadBeeldenVerzoek ActiveMq message to batch BK");
		queueTextBericht(mammaUploadBeeldenVerzoekBerichtDestination, "");
	}

	@Override
	public void queueMammaHl7v24RetryDeleteBerichtUitgaand(Long uitnodigingsNr, Long clientId)
	{
		queueMammaHL7v24BerichtUitgaand(MammaHL7v24ORMBerichtStatus.DELETE, clientId, uitnodigingsNr, null, null);
	}

	@Override
	public void queueMammaHL7v24BerichtUitgaand(Client client, MammaHL7v24ORMBerichtStatus status)
	{
		if (status == MammaHL7v24ORMBerichtStatus.DELETE)
		{
			throw new IllegalArgumentException("" + status);
		}
		queueMammaHL7v24BerichtUitgaand(client.getMammaDossier().getLaatsteScreeningRonde(), status);
	}

	@Override
	public void queueMammaHL7v24BerichtUitgaand(MammaScreeningRonde ronde, MammaHL7v24ORMBerichtStatus status)
	{
		queueMammaHL7v24BerichtUitgaand(status, ronde.getDossier().getClient().getId(), ronde.getUitnodigingsNr(), ronde.getLaatsteUitnodiging().getLaatsteAfspraak(),
			ronde.getLaatsteOnderzoek());
	}

	private void queueMammaHL7v24BerichtUitgaand(MammaHL7v24ORMBerichtStatus status, Long clientId, Long accessionNumber, MammaAfspraak laatsteAfspraak,
		MammaOnderzoek laatsteOnderzoek)
	{
		MammaHL7v24OrmBerichtTriggerMetClientDto triggerDto = new MammaHL7v24OrmBerichtTriggerMetClientDto();
		triggerDto.setClientId(clientId);
		triggerDto.setAccessionNumber(accessionNumber.toString());
		if (status != MammaHL7v24ORMBerichtStatus.CANCELLED && status != MammaHL7v24ORMBerichtStatus.DELETE && status != MammaHL7v24ORMBerichtStatus.GOINGTODELETE)
		{
			triggerDto.setLaatsteAfspraakDatum(DateUtil.toLocalDateTime(laatsteAfspraak.getVanaf()));
			triggerDto.setScreeningseenheidCode(laatsteAfspraak.getStandplaatsPeriode().getScreeningsEenheid().getCode());
		}
		if (laatsteOnderzoek != null &&
			(status == MammaHL7v24ORMBerichtStatus.COMPLETED || status == MammaHL7v24ORMBerichtStatus.AUTHORISED || status == MammaHL7v24ORMBerichtStatus.REPORTED))
		{
			triggerDto.setLaatsteOnderzoekAfgerondOpDatum(DateUtil.toLocalDateTime(laatsteOnderzoek.getAfgerondOp()));
			triggerDto.setScreeningseenheidCode(laatsteOnderzoek.getScreeningsEenheid().getCode());
		}
		triggerDto.setUploaded(false);
		queueHL7(status, triggerDto, status == MammaHL7v24ORMBerichtStatus.DELETE ? MammaHL7BerichtType.IMS_ORM_ILM : MammaHL7BerichtType.IMS_ORM);

		LOG.debug("Sending client message to batch BK HL7 queue");
	}

	@Override
	public void queueMammaUploadBeeldenHL7v24BerichtUitgaand(MammaUploadBeeldenPoging uploadBeeldenPoging, MammaHL7v24ORMBerichtStatus status, Date onderzoeksDatum)
	{
		queueMammaUploadBeeldenHL7v24BerichtUitgaand(uploadBeeldenPoging.getAccessionNumber(),
			uploadBeeldenPoging.getUploadBeeldenVerzoek().getScreeningRonde().getDossier().getClient().getId(), status, onderzoeksDatum);
	}

	@Override
	public void queueMammaUploadBeeldenHL7v24BerichtUitgaand(Long accessionNumber, Long clientId, MammaHL7v24ORMBerichtStatus status, Date onderzoeksDatum)
	{
		MammaHL7v24OrmBerichtTriggerMetClientDto triggerDto = new MammaHL7v24OrmBerichtTriggerMetClientDto();
		triggerDto.setClientId(clientId);
		triggerDto.setAccessionNumber(accessionNumber.toString());
		if (status == MammaHL7v24ORMBerichtStatus.SCHEDULED || status == MammaHL7v24ORMBerichtStatus.COMPLETED)
		{
			triggerDto.setLaatsteOnderzoekAfgerondOpDatum(DateUtil.toLocalDateTime(onderzoeksDatum));
		}
		if (status == MammaHL7v24ORMBerichtStatus.SCHEDULED || status == MammaHL7v24ORMBerichtStatus.STARTED || status == MammaHL7v24ORMBerichtStatus.COMPLETED)
		{
			triggerDto.setScreeningseenheidCode("ZHOND");
		}
		triggerDto.setUploaded(true);
		queueHL7(status, triggerDto, status == MammaHL7v24ORMBerichtStatus.DELETE ? MammaHL7BerichtType.IMS_ORM_ILM_UPLOAD_BEELDEN : MammaHL7BerichtType.IMS_ORM_UPLOAD_BEELDEN);

		LOG.debug("Sending client message to batch BK HL7 queue");
	}

	@Override
	public void queueMammaKwaliteitsopnameHL7v24BerichtUitgaand(MammaKwaliteitsopnameDto kwaliteitsopname, MammaHL7v24ORMBerichtStatus status)
	{
		MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto triggerDto = new MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto();
		triggerDto.setType(kwaliteitsopname.getType());
		triggerDto.setScreeningseenheidCode(kwaliteitsopname.getSeCode());
		triggerDto.setReden(kwaliteitsopname.getReden());
		triggerDto.setPatientID(kwaliteitsopname.getPatientID());
		triggerDto.setAccessionNumber(kwaliteitsopname.getAccessionNumber());
		triggerDto.setOnderzoekscode(kwaliteitsopname.getOnderzoekscode());
		queueHL7(status, triggerDto, MammaHL7BerichtType.IMS_ORM_KWALITEITSOPNAME);

		LOG.debug("Sending kwaliteitsopname message to batch BK HL7 queue");
	}

	private void queueHL7(MammaHL7v24ORMBerichtStatus status, MammaAbstractHL7v24OrmBerichtTriggerDto triggerDto, MammaHL7BerichtType berichtType)
	{
		triggerDto.setStatus(status);
		MammaHL7v24Message queueItem = new MammaHL7v24Message();
		try
		{
			queueItem.setDtoJson(objectMapper.writeValueAsString(triggerDto));
			queueItem.setCreateTime(currentDateSupplier.getDate());
			queueItem.setHl7BerichtType(berichtType);
		}
		catch (JsonProcessingException e)
		{
			throw new RuntimeException(e);
		}
		hibernateService.save(queueItem);
	}

	@Override
	public void queueMammaInkomendIMSBericht(String messageId)
	{
		LOG.debug("Sending ActiveMq message to batch BK");
		queueBericht(mammaDestination, messageId);
	}

	@Override
	public void queueMammaPersoonsGegevensGewijzigdImsBericht(Client client)
	{
		try
		{
			MammaHL7v24Message queueItem = new MammaHL7v24Message();
			MammaHL7v24AdtBerichtTriggerDto triggerDto = new MammaHL7v24AdtBerichtTriggerDto();
			triggerDto.setClientId(client.getId());
			triggerDto.setStatus(MammaHL7ADTBerichtType.PERSOONS_GEGEVENS_GEWIJZIGD);
			queueItem.setDtoJson(objectMapper.writeValueAsString(triggerDto));
			queueItem.setCreateTime(currentDateSupplier.getDate());
			queueItem.setHl7BerichtType(MammaHL7BerichtType.IMS_ADT);
			hibernateService.save(queueItem);
			LOG.debug("Sending message to batch BK HL7 queue");
			client.getGbaMutaties()
				.stream()
				.filter(mutatie -> mutatie.getAanvullendeInformatie() != null)
				.forEach(mutatie -> mutatie.setAanvullendeInformatie(mutatie.getAanvullendeInformatie().replace(Constants.MAMMA_IMS_CLIENT_GEGEVENS_GEWIJZIGD_MARKER, "")));
			hibernateService.saveOrUpdateAll(client.getGbaMutaties());
		}
		catch (JsonProcessingException e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public void queueMammaBsnWijzigingenImsBericht(Client client, GbaMutatie mutatie)
	{
		LOG.info(mutatie.getId() + ": " + mutatie.getAanvullendeInformatie());
		Matcher matcher = bsnGewijzigdMarkerPattern.matcher(mutatie.getAanvullendeInformatie());
		if (matcher.find())
		{
			try
			{
				String oudBsn = matcher.group(2);
				String nieuweBsn = matcher.group(3);

				MammaHL7v24Message queueItem = new MammaHL7v24Message();
				MammaHL7v24AdtBerichtTriggerDto triggerDto = new MammaHL7v24AdtBerichtTriggerDto();
				triggerDto.setOudBsn(oudBsn);
				triggerDto.setNieuweBsn(nieuweBsn);
				triggerDto.setStatus(MammaHL7ADTBerichtType.BSN_GEWIJZIGD);
				triggerDto.setClientId(client.getId());

				queueItem.setDtoJson(objectMapper.writeValueAsString(triggerDto));
				queueItem.setCreateTime(currentDateSupplier.getDate());
				queueItem.setHl7BerichtType(MammaHL7BerichtType.IMS_ADT);
				LOG.debug("Sending message to batch BK HL7 queue");
				String mammaBsnMarkerMetBsnNummers = String.format("|%s:%s,%s|", Constants.MAMMA_IMS_CLIENT_BSN_GEWIJZIGD_MARKER, oudBsn, nieuweBsn);
				mutatie.setAanvullendeInformatie(mutatie.getAanvullendeInformatie().replace(mammaBsnMarkerMetBsnNummers, ""));
				hibernateService.saveOrUpdateAll(queueItem, mutatie);
			}
			catch (JsonProcessingException e)
			{
				throw new RuntimeException(e);
			}
		}
		else
		{
			throw new IllegalStateException("Geen BSN's gevonden bij MAMMA_IMS_CLIENT_BSN_GEWIJZIGD_MARKER");
		}
	}

	private void queueBericht(Destination destination, final Long id)
	{
		jmsTemplate.send(destination, session -> ActiveMQHelper.getActiveMqObjectMessage(id));
	}

	private void queueBericht(Destination destination, final String id)
	{
		jmsTemplate.send(destination, session -> ActiveMQHelper.getActiveMqObjectMessage(id));
	}

	private void queueBericht(Destination destination)
	{
		jmsTemplate.send(destination, session -> new ActiveMQMessage());
	}

	private <T extends Serializable> void queueTextBericht(Destination destination, final T object)
	{
		jmsTemplate.send(destination, session -> ActiveMQHelper.getActiveMqTextMessage(object));
	}
}
