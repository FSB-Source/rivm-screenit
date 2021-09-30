package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.config.MammaHL7ConnectieContext;
import nl.rivm.screenit.batch.dao.MammaHL7v24SendBerichtenQueueDao;
import nl.rivm.screenit.batch.exception.HL7CreateMessageException;
import nl.rivm.screenit.batch.exception.HL7SendMessageException;
import nl.rivm.screenit.batch.model.enums.MammaHL7Connectie;
import nl.rivm.screenit.batch.service.HL7BaseSendMessageService;
import nl.rivm.screenit.batch.service.MammaHL7v24SendService;
import nl.rivm.screenit.dto.mamma.MammaHL7v24AdtBerichtTriggerDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24BerichtTriggerDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerIlmDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetClientDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerUploadBeeldenDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.MammaHl7v24BerichtLogEvent;
import nl.rivm.screenit.model.mamma.MammaHL7v24Message;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.SessionFactoryUtils;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.fasterxml.jackson.databind.ObjectMapper;

import ca.uhn.hl7v2.HL7Exception;

@Service
public class HL7v24BerichtenQueueSenderServiceImpl
{
	private static final Logger LOG = LoggerFactory.getLogger(HL7v24BerichtenQueueSenderServiceImpl.class);

	private static final long MILLIS_WAIT_TIME = TimeUnit.SECONDS.toMillis(10);

	@Autowired
	private MammaHL7v24SendService hl7SendService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaHL7v24SendBerichtenQueueDao hl7SendBerichtenQueueDao;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private HL7BaseSendMessageService sendMessageService;

	@Autowired
	private SessionFactory sessionFactory;

	@Autowired
	private LogService logService;

	private final ObjectMapper objectMapper = new ObjectMapper();

	private final static int MAMMA_IMS_QUEUE_VERWERK_SIZE = 500;

	private int queueWarningThreshold;

	private boolean queueSizeWarning = false;

	private boolean verstuurProblemen = false;

	private long imsSendRunCounter;

	@PostConstruct
	public void init()
	{
		LOG.info("Starting HL7 berichten queue verwerker");
		loadPreferences();
		handleHL7v24Berichten();
	}

	private void loadPreferences()
	{
		queueWarningThreshold = preferenceService.getInteger(PreferenceKey.MAMMA_IMS_QUEUE_SIZE_WARNING_THRESHOLD.name(), 5000);
		hl7SendService.verversConfiguratie();
	}

	private void handleHL7v24Berichten()
	{
		ExecutorService executorService = Executors.newSingleThreadExecutor();
		executorService.submit(() -> {
			while (true)
			{
				MammaHL7ConnectieContext connectieContext = new MammaHL7ConnectieContext();

				try
				{
					boolean skipWait;

					Session session = this.sessionFactory.openSession();
					TransactionSynchronizationManager.bindResource(this.sessionFactory, new SessionHolder(session));
					skipWait = verwerkIMSBerichtenQueue(connectieContext);
					unbindSessionFactory();

					if (!skipWait)
					{
						closeHl7Connections(connectieContext);

						imsSendRunCounter++;
						if (imsSendRunCounter % 6 == 0)
						{
							LOG.info("Running HL7 berichten queue");
							loadPreferences();
						}
						try
						{
							Thread.sleep(MILLIS_WAIT_TIME);
						}
						catch (InterruptedException e)
						{
							LOG.error(String.format("HL7 berichten queue error: %s", e.getMessage()), e);
						}
					}

				}
				catch (Exception e)
				{
					LOG.error("Runtime exceptie tijden het versturen van HL7v24 berichten.", e);
					LogEvent logEvent = new LogEvent();
					logEvent.setLevel(Level.ERROR);
					logEvent.setMelding("Er is een onbekende fout opgetreden tijden het versturen van HL7v24 berichten, neem contact op met Topicus.");
					logService.logGebeurtenis(LogGebeurtenis.MAMMA_HL7_BERICHT_BATCH_GESTOPT, logEvent, Bevolkingsonderzoek.MAMMA);
					return;
				}
				finally
				{
					closeHl7Connections(connectieContext);
				}
			}
		});

	}

	private void unbindSessionFactory()
	{
		if (this.sessionFactory != null)
		{
			SessionHolder sessionHolder = (SessionHolder) TransactionSynchronizationManager.unbindResource(this.sessionFactory);
			SessionFactoryUtils.closeSession(sessionHolder.getSession());
		}
	}

	private boolean verwerkIMSBerichtenQueue(MammaHL7ConnectieContext connectionContext)
	{
		List<MammaHL7v24Message> mammaHL7v24Messages = hl7SendBerichtenQueueDao.fetchMessageQueueForIMS(MAMMA_IMS_QUEUE_VERWERK_SIZE);
		Long queueSize = hl7SendBerichtenQueueDao.fetchQueueSize();
		if (!mammaHL7v24Messages.isEmpty())
		{
			loadPreferences();
			try
			{
				verstuurBerichten(mammaHL7v24Messages, connectionContext);
			}
			catch (HL7Exception e)
			{
				logVerstuurProblemen(e);
			}
		}
		logQueueSizeProblemen(queueSize);
		return !verstuurProblemen && queueSize > MAMMA_IMS_QUEUE_VERWERK_SIZE;
	}

	private void verstuurBerichten(List<MammaHL7v24Message> hl7v24Messages, MammaHL7ConnectieContext connectionContext)
		throws HL7Exception
	{
		for (MammaHL7v24Message hl7v24Message : hl7v24Messages)
		{
			MammaHL7v24BerichtTriggerDto triggerDto = null;
			try
			{
				MammaHL7Connectie messageConnection = connectionContext.getConnectie(hl7v24Message);
				switch (hl7v24Message.getHl7BerichtType())
				{
				case IMS_ORM:
					triggerDto = objectMapper.readValue(hl7v24Message.getDtoJson(), MammaHL7v24OrmBerichtTriggerMetClientDto.class);
					hl7SendService.sendClientORMMessage((MammaHL7v24OrmBerichtTriggerMetClientDto) triggerDto, messageConnection);
					break;
				case IMS_ORM_ILM:
					triggerDto = objectMapper.readValue(hl7v24Message.getDtoJson(), MammaHL7v24OrmBerichtTriggerIlmDto.class);
					hl7SendService.sendClientORMMessage((MammaHL7v24OrmBerichtTriggerIlmDto) triggerDto, messageConnection);
					break;
				case IMS_ORM_KWALITEITSOPNAME:
					triggerDto = objectMapper.readValue(hl7v24Message.getDtoJson(), MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto.class);
					hl7SendService.sendKwaliteitsopnameORMMessage((MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto) triggerDto, messageConnection);
					break;
				case IMS_ORM_UPLOAD_BEELDEN:
				case IMS_ORM_ILM_UPLOAD_BEELDEN:
					triggerDto = objectMapper.readValue(hl7v24Message.getDtoJson(), MammaHL7v24OrmBerichtTriggerUploadBeeldenDto.class);
					hl7SendService.sendUploadBeeldenORMMessage((MammaHL7v24OrmBerichtTriggerUploadBeeldenDto) triggerDto, messageConnection);
					break;
				case IMS_ADT:
					triggerDto = objectMapper.readValue(hl7v24Message.getDtoJson(), MammaHL7v24AdtBerichtTriggerDto.class);
					hl7SendService.sendADTBericht((MammaHL7v24AdtBerichtTriggerDto) triggerDto, messageConnection);
					break;
				default:
					throw new IllegalStateException("Onbekend IMS bericht type gevonden: " + hl7v24Message.getHl7BerichtType());
				}
				hibernateService.delete(hl7v24Message);
			}
			catch (HL7SendMessageException | HL7CreateMessageException | IOException e)
			{
				if (triggerDto != null)
				{
					logVerstuurProblemen(hl7v24Message, triggerDto, e);
				}
				else
				{
					logVerstuurProblemen(new HL7Exception(e));
				}
				LOG.error(String.format("HL7v24 berichten queue error: %s", e.getMessage()));
				return;
			}

		}
		logVerstuurProblemenOpgelost();
	}

	private void closeHl7Connections(MammaHL7ConnectieContext connectionContext)
	{
		for (MammaHL7Connectie connectie : connectionContext.getConnecties())
		{
			if (connectie.getMessageContext().getConnection() != null)
			{
				sendMessageService.discardConnection(connectie.getMessageContext());
			}
		}
	}

	private void logVerstuurProblemenOpgelost()
	{
		if (verstuurProblemen)
		{
			MammaHl7v24BerichtLogEvent logEvent = new MammaHl7v24BerichtLogEvent();
			logEvent.setLevel(Level.INFO);
			logEvent.setMelding("IMS berichten worden weer succesvol verstuurd");
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_HL7_BERICHT_VERBINDING_HERSTELD, logEvent, Bevolkingsonderzoek.MAMMA);
		}
		verstuurProblemen = false;
	}

	private void logVerstuurProblemen(HL7Exception exception)
	{
		if (!verstuurProblemen)
		{
			LOG.error(exception.getMessage(), exception);
			LogEvent logEvent = new LogEvent();
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding("Onbekende fout bij het ophalen van het te versturen HL7v24 bericht");
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_HL7_BERICHT_VERSTUREN_MISLUKT, logEvent, Bevolkingsonderzoek.MAMMA);
		}
		verstuurProblemen = true;
	}

	private void logVerstuurProblemen(MammaHL7v24Message mammaHL7v24Message, MammaHL7v24BerichtTriggerDto hl7BerichtTriggerDto, Exception e)
	{
		if (!verstuurProblemen)
		{
			String messageStructure = e instanceof HL7SendMessageException ? ((HL7SendMessageException) e).getHl7Message() : "";
			MammaHl7v24BerichtLogEvent logEvent = new MammaHl7v24BerichtLogEvent();
			logEvent.setClient(getClient(hl7BerichtTriggerDto));
			logEvent.setHl7MessageStructure(messageStructure);
			logEvent.setLevel(Level.ERROR);
			logEvent.setMelding(String.format("IMS bericht kon niet worden afgeleverd. %s. %s", mammaHL7v24Message.getHl7BerichtType(), e.getMessage()));
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_HL7_BERICHT_VERSTUREN_MISLUKT, logEvent, null, getClient(hl7BerichtTriggerDto), Bevolkingsonderzoek.MAMMA);
		}
		verstuurProblemen = true;
	}

	private Client getClient(MammaHL7v24BerichtTriggerDto triggerDto)
	{
		if (triggerDto instanceof MammaHL7v24OrmBerichtTriggerMetClientDto)
		{
			final Long clientId = ((MammaHL7v24OrmBerichtTriggerMetClientDto) triggerDto).getClientId();
			if (clientId != null)
			{
				return hibernateService.load(Client.class, clientId);
			}
		}
		return null;
	}

	private void logQueueSizeProblemen(Long queueSize)
	{
		boolean oldQueueSizeWarningValue = queueSizeWarning;
		queueSizeWarning = queueSize > queueWarningThreshold;
		if (oldQueueSizeWarningValue != queueSizeWarning)
		{
			if (queueSizeWarning)
			{
				LOG.info("Queue size wordt te groot!");
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_HL7_BERICHT_QUEUE_ERG_GROOT,
					new LogEvent(String.format("Er staan meer dan %d berichten in de queue", queueWarningThreshold)), Bevolkingsonderzoek.MAMMA);
			}
			else
			{
				LOG.info("Queue size wordt weer klein genoeg");
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_HL7_BERICHT_QUEUE_NORMAAL,
					new LogEvent("Het aantal berichten in de queue is weer normaal."), Bevolkingsonderzoek.MAMMA);
			}
		}
	}

}
