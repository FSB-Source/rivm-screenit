package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.exception.HL7CreateMessageException;
import nl.rivm.screenit.batch.model.HL7v24ResponseWrapper;
import nl.rivm.screenit.batch.model.ScreenITHL7MessageContext;
import nl.rivm.screenit.batch.model.enums.MammaHL7Connectie;
import nl.rivm.screenit.batch.service.HL7BaseSendMessageService;
import nl.rivm.screenit.batch.service.MammaHL7CreateMessageService;
import nl.rivm.screenit.batch.service.MammaHL7v24SendService;
import nl.rivm.screenit.dto.mamma.MammaHL7v24AdtBerichtTriggerDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetClientDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto;
import nl.rivm.screenit.exceptions.HL7SendMessageException;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.berichten.ScreenITResponseV24MessageWrapper;
import nl.rivm.screenit.model.mamma.enums.MammaHL7ADTBerichtType;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ca.uhn.hl7v2.AcknowledgmentCode;
import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.app.Connection;
import ca.uhn.hl7v2.llp.LLPException;
import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.model.v24.message.ADT_AXX;
import ca.uhn.hl7v2.model.v24.message.ORM_O01;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaHL7v24SendServiceImpl implements MammaHL7v24SendService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaHL7v24SendServiceImpl.class);

	@Autowired
	private HL7BaseSendMessageService sendMessageService;

	@Autowired
	private MammaHL7CreateMessageService hl7CreateMessageService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private HibernateService hibernateService;

	private String hl7IMSHost;

	private String hl7IMSADTPort;

	private String hl7IMSORMPort;

	private String hl7ImsOrmIlmPort;

	private boolean hl7Enabled = Boolean.TRUE;

	@Override
	public void verversConfiguratie()
	{
		hl7IMSHost = preferenceService.getString(PreferenceKey.MAMMA_IMS_HOST_NAME.name());
		hl7IMSORMPort = preferenceService.getString(PreferenceKey.MAMMA_IMS_ORM_PORT.name());
		hl7ImsOrmIlmPort = preferenceService.getString(PreferenceKey.MAMMA_IMS_ORM_ILM_PORT.name());
		hl7IMSADTPort = preferenceService.getString(PreferenceKey.MAMMA_IMS_ADT_PORT.name());

		hl7Enabled = preferenceService.getBoolean(PreferenceKey.MAMMA_HL7_ENABLED.name(), true);
	}

	@Override
	public void sendADTBericht(MammaHL7v24AdtBerichtTriggerDto adtBerichtTriggerDto, MammaHL7Connectie messageConnection)
		throws HL7CreateMessageException, HL7SendMessageException
	{
		LOG.info("Creating HL7v24 ADT message {} for client id {}", adtBerichtTriggerDto.getStatus(), adtBerichtTriggerDto.getClientId());
		Client client = hibernateService.load(Client.class, adtBerichtTriggerDto.getClientId());
		reloadClientData(client);
		Message bericht = createADTMessage(client, adtBerichtTriggerDto);
		sendMessageAndHandleResponse(bericht, messageConnection);
	}

	private void sendMessageAndHandleResponse(Message bericht, MammaHL7Connectie messageConnection) throws HL7SendMessageException
	{
		try
		{
			HL7v24ResponseWrapper responseWrapper = sendMammaHL7Message(bericht, messageConnection);
			ScreenITResponseV24MessageWrapper responseV24MessageWrapper = responseWrapper.getResponseV24MessageWrapper();
			handleIMSResponse(responseV24MessageWrapper, bericht);
		}
		catch (HL7Exception | HL7SendMessageException | LLPException | IOException e)
		{
			LOG.error("Fout bij het versturen van HL7 bericht", e);
			throw new HL7SendMessageException(e.getMessage(), bericht.toString(), e);
		}
	}

	@Override
	public void sendClientORMMessage(MammaHL7v24OrmBerichtTriggerMetClientDto hl7BerichtTrigger, MammaHL7Connectie messageConnection)
		throws HL7CreateMessageException, HL7SendMessageException
	{
		LOG.info("Creating HL7v24 ORM message {} for client id {} {}", hl7BerichtTrigger.getStatus().getLabel(), hl7BerichtTrigger.getClientId(),
			(hl7BerichtTrigger.isUploaded() ? "ihkv. upload beelden" : ""));
		Client client = hibernateService.load(Client.class, hl7BerichtTrigger.getClientId());
		reloadClientData(client);
		Message bericht = createORMMessage(client, hl7BerichtTrigger);
		sendMessageAndHandleResponse(bericht, messageConnection);
	}

	@Override
	public void sendKwaliteitsopnameORMMessage(MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto hl7BerichtTrigger, MammaHL7Connectie messageConnection)
		throws HL7CreateMessageException
	{
		LOG.info("Creating HL7v24 ORM message {} for kwaliteitsopname", hl7BerichtTrigger.getStatus().getLabel());
		try
		{
			Message bericht = hl7CreateMessageService.maakKwaliteitsopnameORMBericht(hl7BerichtTrigger);
			sendMessageAndHandleResponse(bericht, messageConnection);
		}
		catch (Exception e)
		{
			String melding = "Fout bij aanmaken HL7 ORM bericht!";
			LOG.error(melding, e);
			throw new HL7CreateMessageException(melding, e);
		}
	}

	private void reloadClientData(Client client)
	{
		client.getMammaDossier().getId(); 
		hibernateService.getHibernateSession().evict(client);
		hibernateService.reload(client.getMammaDossier());
	}

	private void handleIMSResponse(ScreenITResponseV24MessageWrapper responseV24MessageWrapper, Message message) throws HL7SendMessageException
	{
		if (hl7Enabled && !responseV24MessageWrapper.getAcknowledgmentCode().equals(AcknowledgmentCode.AA))
		{
			String melding = String.format("Geen ACK ontvangen! %s.", responseV24MessageWrapper.getMelding());
			LOG.error(melding);
			throw new HL7SendMessageException(melding, message.toString());
		}
	}

	private ADT_AXX createADTMessage(Client client, MammaHL7v24AdtBerichtTriggerDto adtBerichtTriggerDto) throws HL7CreateMessageException
	{
		try
		{
			if (adtBerichtTriggerDto.getStatus() == MammaHL7ADTBerichtType.BSN_GEWIJZIGD)
			{
				return hl7CreateMessageService.maakADTBerichtGewijzigdBsn(client, adtBerichtTriggerDto.getOudBsn(), adtBerichtTriggerDto.getNieuweBsn());
			}
			else if (adtBerichtTriggerDto.getStatus() == MammaHL7ADTBerichtType.PERSOONS_GEGEVENS_GEWIJZIGD)
			{
				return hl7CreateMessageService.maakADTBerichtPersoonsgegevensGewijzigd(client);
			}
		}
		catch (HL7Exception | IOException e)
		{
			String melding = "Fout bij aanmaken HL7 ORM bericht!";
			LOG.error(melding, e);
			throw new HL7CreateMessageException(melding, e);
		}
		throw new IllegalStateException("Geen valide ADT bericht status");
	}

	private ORM_O01 createORMMessage(Client client, MammaHL7v24OrmBerichtTriggerMetClientDto hl7BerichtTrigger) throws HL7CreateMessageException
	{
		try
		{
			return hl7CreateMessageService.maakClientORMBericht(hl7BerichtTrigger, client);
		}
		catch (Exception e)
		{
			String melding = "Fout bij aanmaken HL7 ORM bericht!";
			LOG.error(melding, e);
			throw new HL7CreateMessageException(melding, e);
		}
	}

	private void openHl7Connection(MammaHL7Connectie hl7Connection) throws HL7Exception
	{
		String host = null;
		String port = null;
		switch (hl7Connection)
		{
		case ADT:
			port = hl7IMSADTPort;
			host = hl7IMSHost;
			break;
		case ORM:
			port = hl7IMSORMPort;
			host = hl7IMSHost;
			break;
		case ORM_ILM:
			port = hl7ImsOrmIlmPort;
			host = hl7IMSHost;
		}
		ScreenITHL7MessageContext messageContext = hl7Connection.getMessageContext();
		messageContext.setHost(host);
		messageContext.setPort(Integer.valueOf(port));
		sendMessageService.openConnection(hl7Connection.getConnectieNaam(), 3, messageContext);
	}

	private HL7v24ResponseWrapper sendMammaHL7Message(Message hl7Bericht, MammaHL7Connectie messageConnection)
		throws HL7Exception, LLPException, IOException
	{
		ScreenITHL7MessageContext messageContext = messageConnection.getMessageContext();
		if (hl7Enabled)
		{
			Connection connection = messageContext.getConnection();
			if (connection == null)
			{
				openHl7Connection(messageConnection);
			}
			connection = messageContext.getConnection(); 
			LOG.info(String.format("Sending message to %s:%d", connection.getRemoteAddress().getHostName(), connection.getRemotePort()));
			sendMessageService.sendHL7Message(hl7Bericht, messageContext);
		}
		else
		{
			LOG.warn("HL7 berichten versturen staat uit.");
			return new HL7v24ResponseWrapper();
		}
		return messageContext.getResponseWrapper();
	}
}
