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
import java.util.List;
import java.util.concurrent.TimeUnit;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.config.MammaHL7ConnectieContext;
import nl.rivm.screenit.batch.dao.MammaHL7v24SendBerichtenQueueDao;
import nl.rivm.screenit.batch.exception.HL7CreateMessageException;
import nl.rivm.screenit.batch.model.enums.MammaHL7Connectie;
import nl.rivm.screenit.batch.service.HL7BaseSendMessageService;
import nl.rivm.screenit.batch.service.MammaHL7v24SendService;
import nl.rivm.screenit.dto.mamma.MammaHL7v24AdtBerichtTriggerDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24BerichtTriggerDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetClientDto;
import nl.rivm.screenit.dto.mamma.MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto;
import nl.rivm.screenit.exceptions.HL7SendMessageException;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.MammaHl7v24BerichtLogEvent;
import nl.rivm.screenit.model.mamma.MammaHL7v24Message;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.orm.hibernate5.SessionFactoryUtils;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.fasterxml.jackson.databind.ObjectMapper;

import ca.uhn.hl7v2.HL7Exception;

@Slf4j
@Configuration
@EnableScheduling
public class HL7v24BerichtenQueueSenderServiceImpl
{
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

	private static final ObjectMapper objectMapper = new ObjectMapper();

	private static final int MAMMA_IMS_QUEUE_VERWERK_SIZE = 500;

	private static final long VERSTUURPROBLEMEN_DELAY = TimeUnit.SECONDS.toMillis(10);

	private static final long VERSTUURPROBLEMEN_MAX_DELAY = TimeUnit.MINUTES.toMillis(2);

	private int queueWarningThreshold;

	private static final ThreadLocal<Boolean> bindSessionOnThread = new ThreadLocal<>();

	private boolean queueSizeWarning = false;

	private boolean erZijnVerstuurProblemen = false;

	private int verstuurProblemenCount = 0;

	private long imsSendRunCounter;

	private boolean doInit = true;

	private boolean mailGestuurdBijError = false;

	private boolean laatsteRunZonderProblemen = true;

	@Scheduled(fixedDelay = 10000)
	public void handleHL7v24Berichten()
	{
		MammaHL7ConnectieContext connectieContext = null;
		try
		{
			wachtBijRuntimeProblemen();
			onInit();
			connectieContext = new MammaHL7ConnectieContext();

			boolean nogBerichtenOpQueue;

			do
			{
				bindSession();
				nogBerichtenOpQueue = verwerkIMSBerichtenQueue(connectieContext);
				unbindSessionFactory();
			}
			while (nogBerichtenOpQueue);

			closeHl7Connections(connectieContext);

			imsSendRunCounter++;
			if (imsSendRunCounter >= 6)
			{
				LOG.info("Running HL7 berichten queue");
				bindSession();
				loadPreferences();
				unbindSessionFactory();
				imsSendRunCounter = 0;
			}
			logVerstuurProblemenOpgelost();
		}
		catch (InterruptedException interruptedException)
		{
			LOG.error("InterruptException in berichten behandelen", interruptedException);
			Thread.currentThread().interrupt();
		}
		catch (Exception exception)
		{
			if (connectieContext != null)
			{
				closeHl7Connections(connectieContext);
			}
			handleRuntimeException(exception);
		}
	}

	private void onInit()
	{
		if (doInit)
		{
			LOG.info("Starting HL7 berichten queue verwerker");
			bindSession();
			loadPreferences();
			unbindSessionFactory();

			doInit = false;
		}
	}

	private void loadPreferences()
	{
		queueWarningThreshold = preferenceService.getInteger(PreferenceKey.MAMMA_IMS_QUEUE_SIZE_WARNING_THRESHOLD.name(), 5000);
		hl7SendService.verversConfiguratie();
	}

	private void bindSession()
	{
		if (!Boolean.TRUE.equals(bindSessionOnThread.get()))
		{
			Session session = sessionFactory.openSession();
			TransactionSynchronizationManager.bindResource(sessionFactory, new SessionHolder(session));
			bindSessionOnThread.set(true);
		}
	}

	private void unbindSessionFactory()
	{
		if (Boolean.TRUE.equals(bindSessionOnThread.get()))
		{
			SessionHolder sessionHolder = (SessionHolder) TransactionSynchronizationManager.unbindResource(sessionFactory);
			SessionFactoryUtils.closeSession(sessionHolder.getSession());
			bindSessionOnThread.remove();
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
		return !erZijnVerstuurProblemen && queueSize > MAMMA_IMS_QUEUE_VERWERK_SIZE;
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
				case IMS_ORM_ILM:
				case IMS_ORM_UPLOAD_BEELDEN:
				case IMS_ORM_ILM_UPLOAD_BEELDEN:
					triggerDto = objectMapper.readValue(hl7v24Message.getDtoJson(), MammaHL7v24OrmBerichtTriggerMetClientDto.class);
					hl7SendService.sendClientORMMessage((MammaHL7v24OrmBerichtTriggerMetClientDto) triggerDto, messageConnection);
					break;
				case IMS_ORM_KWALITEITSOPNAME:
					triggerDto = objectMapper.readValue(hl7v24Message.getDtoJson(), MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto.class);
					hl7SendService.sendKwaliteitsopnameORMMessage((MammaHL7v24OrmBerichtTriggerMetKwaliteitsopnameDto) triggerDto, messageConnection);
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
		if (laatsteRunZonderProblemen)
		{
			if (erZijnVerstuurProblemen)
			{
				String melding = "IMS berichten worden weer succesvol verstuurd.";
				LOG.info(melding);

				logService.logGebeurtenis(LogGebeurtenis.MAMMA_HL7_BERICHT_VERBINDING_HERSTELD, null, melding, Bevolkingsonderzoek.MAMMA);
				mailGestuurdBijError = false;
				verstuurProblemenCount = 0;
			}
			erZijnVerstuurProblemen = false;
		}
		laatsteRunZonderProblemen = true;
	}

	private void logVerstuurProblemen(HL7Exception exception)
	{
		if (!erZijnVerstuurProblemen)
		{
			LOG.error(exception.getMessage(), exception);
			String melding = "Onbekende fout bij het ophalen van het te versturen HL7v24 bericht";

			maakEnLogErrorLogEvent(LogGebeurtenis.MAMMA_HL7_BERICHT_VERSTUREN_MISLUKT, melding);
		}
		erZijnVerstuurProblemen = true;
		laatsteRunZonderProblemen = false;
	}

	private void logVerstuurProblemen(MammaHL7v24Message mammaHL7v24Message, MammaHL7v24BerichtTriggerDto hl7BerichtTriggerDto, Exception e)
	{
		if (!erZijnVerstuurProblemen)
		{
			String messageStructure = e instanceof HL7SendMessageException ? ((HL7SendMessageException) e).getHl7Message() : "";
			String melding = String.format("IMS bericht kon niet worden afgeleverd. %s. %s", mammaHL7v24Message.getHl7BerichtType(), e.getMessage());

			maakEnLogErrorLogEvent(getClient(hl7BerichtTriggerDto), messageStructure, LogGebeurtenis.MAMMA_HL7_BERICHT_VERSTUREN_MISLUKT, melding);
		}
		erZijnVerstuurProblemen = true;
		laatsteRunZonderProblemen = false;
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

	private void handleRuntimeException(Exception e)
	{
		setVerstuurProblemenEnCount();

		LOG.error("Runtime exceptie tijden het versturen van HL7v24 berichten.", e);

		String logMelding = "Er is een onbekende fout opgetreden tijden het versturen van HL7v24 berichten, neem contact op met Topicus. Het systeem probeert zichzelf te herstellen.";
		bindSession();
		maakEnLogErrorLogEvent(LogGebeurtenis.MAMMA_HL7_BERICHT_BATCH_GESTOPT, logMelding);

		unbindSessionFactory();
	}

	private void setVerstuurProblemenEnCount()
	{
		erZijnVerstuurProblemen = true;
		if ((verstuurProblemenCount * VERSTUURPROBLEMEN_DELAY) < VERSTUURPROBLEMEN_MAX_DELAY)
		{
			verstuurProblemenCount++;
		}
	}

	private void wachtBijRuntimeProblemen() throws InterruptedException
	{
		if (erZijnVerstuurProblemen && verstuurProblemenCount > 0)
		{
			var wachttijd = verstuurProblemenCount * VERSTUURPROBLEMEN_DELAY;
			LOG.info("Er zijn problemen met de verwerking van de HL7v24 berichten, wacht {} seconden extra.", wachttijd / 1000);
			Thread.sleep(wachttijd);
		}
	}

	private void maakEnLogErrorLogEvent(Client client, String messageStructure, LogGebeurtenis logGebeurtenis, String melding)
	{
		if (!mailGestuurdBijError)
		{
			var logEvent = new MammaHl7v24BerichtLogEvent();
			logEvent.setHl7MessageStructure(messageStructure);
			logEvent.setMelding(melding);

			logService.logGebeurtenis(logGebeurtenis, logEvent, null, client, Bevolkingsonderzoek.MAMMA);
			mailGestuurdBijError = true;
		}
	}

	private void maakEnLogErrorLogEvent(LogGebeurtenis logGebeurtenis, String melding)
	{
		if (!mailGestuurdBijError)
		{
			logService.logGebeurtenis(logGebeurtenis, null, melding, Bevolkingsonderzoek.MAMMA);
			mailGestuurdBijError = true;
		}
	}
}
