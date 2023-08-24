package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.model.HL7v24ResponseWrapper;
import nl.rivm.screenit.batch.model.HapiContextType;
import nl.rivm.screenit.batch.model.ScreenITHL7MessageContext;
import nl.rivm.screenit.batch.service.CervixHL7BaseService;
import nl.rivm.screenit.batch.service.CervixOrderBerichtService;
import nl.rivm.screenit.batch.service.HL7BaseSendMessageService;
import nl.rivm.screenit.exceptions.HL7v2ConnectionException;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.berichten.CervixFoutHL7v2Bericht;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.dto.CervixHL7v24HpvOrderTriggerDto;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MessageService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.orm.hibernate5.SessionFactoryUtils;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import ca.uhn.hl7v2.HL7Exception;
import ca.uhn.hl7v2.app.TimeoutException;
import ca.uhn.hl7v2.llp.LLPException;

@Slf4j
@Configuration
@EnableScheduling
public class CervixHl7v2TriggerMessageQueue
{

	@Autowired
	private CervixOrderBerichtService orderBerichtService;

	@Autowired
	private SessionFactory sessionFactory;

	@Autowired
	private MessageService messageService;

	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private HL7BaseSendMessageService sendMessageService;

	@Autowired
	private CervixHL7BaseService hl7BaseService;

	@Autowired
	private InstellingService instellingService;

	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private static final long MILLIS_WAIT_TIME = TimeUnit.SECONDS.toMillis(10);

	private static final long MAX_CONNECTING_RETRY_TIME = TimeUnit.HOURS.toMillis(3);

	private static final long CONNECTING_RETRY_TIME_SHORT = TimeUnit.SECONDS.toMillis(5);

	private static final long CONNECTING_RETRY_TIME_LONG = TimeUnit.MINUTES.toMillis(5);

	private static final long MAX_SEND_RETRY_TIME = TimeUnit.MINUTES.toMillis(5);

	private static final long SEND_RETY_TIME = TimeUnit.SECONDS.toMillis(5);

	private static final int QUEUE_VERWERK_SIZE = 500;

	private static final int QUEUE_WARNING_THRESHOLD = 5000;

	private final ThreadLocal<Boolean> bindSessionOnThread = new ThreadLocal<>();

	private final ExecutorService EXECUTOR_SERVICE = Executors.newFixedThreadPool(8);

	private final Map<Long, CervixHl7v2TriggerMessagePerLabQueue> queueMap = new HashMap<>();

	@Scheduled(cron = "0/10 * * * * *")
	public void verstuurHpvOrderBerichten()
	{
		List<Long> teVerwijderenQueues = new ArrayList<>();
		queueMap.entrySet().stream().filter(lq -> lq.getValue().isStopped()).forEach(lq ->
			{
				teVerwijderenQueues.add(lq.getKey());
				try
				{
					lq.getValue().join(1000L);
				}
				catch (InterruptedException e)
				{
					throw new RuntimeException(e);
				}
			}
		);
		teVerwijderenQueues.forEach(queueMap::remove);

		bindSession();
		var labs = instellingService.getActieveInstellingen(BMHKLaboratorium.class);

		labs.stream()
			.filter(lab -> !queueMap.keySet().contains(lab.getId()))
			.filter(lab -> organisatieParameterService.getOrganisatieParameter(lab, OrganisatieParameterKey.CERVIX_HPV_ORDER_NIEUW, Boolean.FALSE))
			.forEach(lab ->
			{
				var thread = new CervixHl7v2TriggerMessagePerLabQueue(lab);
				queueMap.put(lab.getId(), thread);
				try
				{
					EXECUTOR_SERVICE.submit(queueMap.get(lab.getId()));
					LOG.info("Added HL7v2 verstuur thread voor {}", lab.getNaam());
				}
				catch (RejectedExecutionException e)
				{
					LOG.error("Starten van HL7v2 verstuur thread voor {} is niet gelukt.", lab.getNaam(), e);
				}
			});
		unbindSessionFactory();
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

	private class CervixHl7v2TriggerMessagePerLabQueue extends Thread
	{
		private final Long labId;

		private final String labNaam;

		private boolean queueSizeWarning = false;

		private boolean verstuurProblemen = false;

		private long sendRunCounter = 0;

		private List<Long> clientIdsFoutBerichten = new ArrayList<>();

		@Getter
		private boolean stopped = false;

		CervixHl7v2TriggerMessagePerLabQueue(BMHKLaboratorium lab)
		{
			this.labId = lab.getId();
			this.labNaam = lab.getNaam();
		}

		@Override
		public void run()
		{
			LOG.info("Lab {}: Thread started", labNaam);
			while (true)
			{
				var messageContext = new ScreenITHL7MessageContext(HapiContextType.UTF_8);
				try
				{
					boolean skipWait = verwerkMessages(messageContext);

					if (!skipWait)
					{
						closeHl7Connections(messageContext);

						sendRunCounter++;
						if (sendRunCounter % 10 == 0)
						{
							LOG.info("Lab {}: Heartbeat HL7v2 berichten queue", labNaam);
							checkLab();
						}
						Thread.sleep(MILLIS_WAIT_TIME);
					}
				}
				catch (Exception e)
				{
					LOG.error("Lab {}: Fout tijdens het versturen van HL7v2 berichten.", labNaam, e);
					var logMessage = String.format("Lab %s: Er is een onbekende fout opgetreden tijdens het versturen van HL7v2 berichten, neem contact op met Topicus.",
						labNaam);
					if (e instanceof HL7v2ConnectionException)
					{
						logMessage = String.format("Lab %s: %s %s", labNaam, e.getMessage(), e.getCause() != null ? e.getCause().getMessage() : null);
					}
					logService.logGebeurtenis(LogGebeurtenis.CERVIX_HL7V2_BERICHT_BATCH_GESTOPT, null, logMessage, Bevolkingsonderzoek.CERVIX);
					break;
				}
				finally
				{
					closeHl7Connections(messageContext);
					unbindSessionFactory();
				}
			}
			stopped = true;
		}

		private void checkLab()
		{
			bindSession();
			if (getLaboratorium() == null)
			{
				throw new IllegalStateException(String.format("Lab '%s' met id '%s' niet (meer) beschikbaar", labNaam, labId));
			}
			unbindSessionFactory();
		}

		private void closeHl7Connections(ScreenITHL7MessageContext connectionContext)
		{
			sendMessageService.discardConnection(connectionContext);
		}

		private boolean verwerkMessages(ScreenITHL7MessageContext messageContext) throws HL7v2ConnectionException, InterruptedException
		{
			bindSession();
			List<Long> messageIds = messageService.fetchMessages(MessageType.HPV_ORDER, labId.toString(), QUEUE_VERWERK_SIZE).stream().map(Message::getId)
				.collect(Collectors.toList());
			var queueSize = messageService.fetchQueueSize(MessageType.HPV_ORDER, labId.toString());
			unbindSessionFactory();
			LOG.debug("Lab {}: {} order triggers geselecteerd om HPV orders ter versturen", labNaam, queueSize);
			if (!messageIds.isEmpty())
			{
				try
				{
					openConnection(messageContext);
					verwerkLabMessageTriggers(messageIds, messageContext);
				}
				catch (HL7Exception e)
				{
					logVerstuurProblemen(e);
				}
				finally
				{
					unbindSessionFactory();
				}
			}
			logQueueSizeProblemen(queueSize);
			return !verstuurProblemen && queueSize > QUEUE_VERWERK_SIZE;
		}

		private void verwerkLabMessageTriggers(List<Long> messageIds, ScreenITHL7MessageContext messageContext) throws HL7Exception
		{
			OpenHibernate5Session.withCommittedTransaction().run(() ->
				clientIdsFoutBerichten = hibernateService.getByParameters(CervixFoutHL7v2Bericht.class, Map.of("laboratorium", labId)).stream()
					.map(b -> b.getClient().getId())
					.collect(Collectors.toList()));
			for (Long messageId : messageIds)
			{
				CervixHL7v24HpvOrderTriggerDto hl7v2HpvOrderTriggerDto = null;
				Message message = null;
				try
				{
					bindSession();
					message = hibernateService.get(Message.class, messageId);
					hl7v2HpvOrderTriggerDto = messageService.getContent(message);
					var monster = getMonster(hl7v2HpvOrderTriggerDto);
					checkConnection(messageContext);
					if (geenFoutBerichtenVoorClient(getClient(monster)) && verwerkHpvOrderTrigger(monster, message, messageContext, hl7v2HpvOrderTriggerDto))
					{
						messageService.dequeueMessage(message);
					}
					unbindSessionFactory();
				}
				catch (Exception e)
				{
					LOG.error("Lab {}: HL7v24 berichten queue error. Reden: {}", labNaam, e.getMessage());
					logVerstuurProblemen(message, hl7v2HpvOrderTriggerDto, e);
					return;
				}
				finally
				{
					unbindSessionFactory();
				}
			}
			logVerstuurProblemenOpgelost();
		}

		private BMHKLaboratorium getLaboratorium()
		{
			return hibernateService.get(BMHKLaboratorium.class, labId);
		}

		private void checkConnection(ScreenITHL7MessageContext messageContext) throws HL7v2ConnectionException
		{
			if (!messageContext.getConnection().isOpen())
			{
				var melding = String.format("Lab %s: Bericht kon niet verzonden worden. Verbinding is gesloten!", labNaam);
				LOG.error(melding);
				throw new HL7v2ConnectionException(melding);
			}
		}

		private boolean verwerkHpvOrderTrigger(CervixMonster monster, Message message, ScreenITHL7MessageContext messageContext,
			CervixHL7v24HpvOrderTriggerDto hl7v2HpvOrderTriggerDto)
			throws HL7Exception, LLPException, IOException, InterruptedException
		{

			var hl7v24RequestBerichtTekst = orderBerichtService.maakHpvOrderTextBericht(monster, hl7v2HpvOrderTriggerDto.isCancelOrder());

			sendHpvOrder(messageContext, hl7v24RequestBerichtTekst);

			var responseWrapper = messageContext.getResponseWrapper();
			if (!responseWrapper.isSuccess())
			{
				var melding = String.format("Lab %s: Bericht kon niet verzonden worden. Reden: %s", labNaam, responseWrapper.getMelding());
				LOG.error(melding, responseWrapper.getCrashException());
				maakFoutBericht(monster, message, responseWrapper, hl7v24RequestBerichtTekst);
				logService.logGebeurtenis(LogGebeurtenis.CERVIX_HL7V2_BERICHT_VERSTUREN_MISLUKT, getClient(monster), melding, Bevolkingsonderzoek.CERVIX);
				return false;
			}
			return true;
		}

		private void sendHpvOrder(ScreenITHL7MessageContext messageContext, String hl7v24RequestBerichtTekst)
			throws HL7Exception, LLPException, IOException, InterruptedException
		{
			var responseWrapper = messageContext.getResponseWrapper();
			var firstTry = true;
			var startSendMessage = System.currentTimeMillis();
			var timedOut = false;
			do
			{
				if (!firstTry)
				{
					Thread.sleep(SEND_RETY_TIME);
				}
				try
				{
					sendMessageService.sendHL7Message(hl7v24RequestBerichtTekst, messageContext);
				}
				catch (TimeoutException e)
				{
					LOG.error("Lab '{}' timeout", labNaam, e);
					if (System.currentTimeMillis() - startSendMessage <= MAX_SEND_RETRY_TIME)
					{
						responseWrapper.setMelding(null); 
					}
				}
				finally
				{
					firstTry = false;

					if (System.currentTimeMillis() - startSendMessage > MAX_SEND_RETRY_TIME && responseWrapper.isError())
					{
						LOG.error("Lab '{}' lukt niet om bericht te versturen. Stop met retry.", labNaam);
						responseWrapper.setMelding(
							String.format("Alleen AE of CE responses na aantal pogingen gedurende %ds.", TimeUnit.MILLISECONDS.toSeconds(MAX_SEND_RETRY_TIME)));
						timedOut = true;
					}
				}
			}
			while (responseWrapper.isError() && !timedOut);
		}

		private void maakFoutBericht(CervixMonster monster, Message message, HL7v24ResponseWrapper responseWrapper, String hl7v24RequestBerichtTekst)
		{
			var foutBericht = new CervixFoutHL7v2Bericht();
			var client = getClient(monster);
			foutBericht.setClient(client);
			foutBericht.setResponseMoment(currentDateSupplier.getLocalDateTime());
			foutBericht.setRequest(hl7v24RequestBerichtTekst);
			foutBericht.setLaboratorium(getLaboratorium());
			foutBericht.setMessage(message);
			var responseV24MessageWrapper = responseWrapper.getResponseV24MessageWrapper();
			if (responseV24MessageWrapper != null)
			{
				foutBericht.setResponse(responseV24MessageWrapper.getAcknowledgmentCodeString() +
					(StringUtils.isNotBlank(responseV24MessageWrapper.getMelding()) ? ":" + responseV24MessageWrapper.getMelding() : ""));
			}
			else
			{
				foutBericht.setRequest(responseWrapper.getMelding());
			}

			hibernateService.saveOrUpdate(foutBericht);
			clientIdsFoutBerichten.add(client.getId());
		}

		private boolean geenFoutBerichtenVoorClient(Client client)
		{
			return !clientIdsFoutBerichten.contains(client.getId());
		}

		private void openConnection(ScreenITHL7MessageContext messageContext) throws HL7v2ConnectionException, InterruptedException
		{
			var connection = messageContext.getConnection();
			if (connection == null)
			{
				boolean connectieProblemenGelogd = false;
				String oldHost = null;
				Integer oldPort = null;
				long starttijdOpzettenConnectie = System.currentTimeMillis();
				while (true)
				{
					try
					{
						oldHost = messageContext.getHost();
						oldPort = messageContext.getPort();
						bindSession();
						BMHKLaboratorium laboratorium = getLaboratorium();
						messageContext.setHost(organisatieParameterService.getOrganisatieParameter(laboratorium, OrganisatieParameterKey.CERVIX_HPV_ORDER_HOST));
						messageContext.setPort(organisatieParameterService.getOrganisatieParameter(laboratorium, OrganisatieParameterKey.CERVIX_HPV_ORDER_PORT));
						hl7BaseService.openConnection(laboratorium.getNaam(), 3, messageContext);
						unbindSessionFactory();
						break;
					}
					catch (HL7Exception | IllegalStateException e)
					{
						handleConnectingException(messageContext, connectieProblemenGelogd, oldHost, oldPort, starttijdOpzettenConnectie, e);
						connectieProblemenGelogd = true;
					}
					finally
					{
						unbindSessionFactory();
					}
				}
				if (connectieProblemenGelogd)
				{
					OpenHibernate5Session.withCommittedTransaction().run(() ->
						logService.logGebeurtenis(LogGebeurtenis.CERVIX_HL7V2_BERICHT_VERBINDING_HERSTELD, null,
							String.format("Lab %s: Verbinding is hersteld. HL7v2 berichten kunnen weer verstuurd worden.", labNaam), Bevolkingsonderzoek.CERVIX));
				}
			}
		}

		private void handleConnectingException(ScreenITHL7MessageContext messageContext, boolean connectieProblemenGelogd, String oldHost, Integer oldPort,
			long starttijdOpzettenConnectie, Exception e) throws InterruptedException, HL7v2ConnectionException
		{
			if (!connectieProblemenGelogd || logMislukteConnection(messageContext, oldHost, oldPort))
			{
				OpenHibernate5Session.withCommittedTransaction()
					.run(() -> logService.logGebeurtenis(LogGebeurtenis.CERVIX_HL7V2_BERICHT_VERSTUREN_MISLUKT, null, e.getMessage(), Bevolkingsonderzoek.CERVIX));
			}
			long connectingRetryTime = CONNECTING_RETRY_TIME_LONG;
			if (e instanceof IllegalStateException)
			{

				connectingRetryTime = CONNECTING_RETRY_TIME_SHORT;
			}
			LOG.error("Lab {}: Connectie problemen. Retry in {} seconden", labNaam, TimeUnit.MILLISECONDS.toSeconds(connectingRetryTime), e);
			Thread.sleep(connectingRetryTime);
			if (System.currentTimeMillis() - starttijdOpzettenConnectie > MAX_CONNECTING_RETRY_TIME)
			{
				throw new HL7v2ConnectionException(
					"Kan geen connectie opzetten binnen " + TimeUnit.MILLISECONDS.toHours(MAX_CONNECTING_RETRY_TIME) + " uur", e);
			}
		}

		private boolean logMislukteConnection(ScreenITHL7MessageContext messageContext, String oldHost, Integer oldPort)
		{
			return oldHost == null && messageContext.getHost() != null
				|| oldHost != null && !oldHost.equals(messageContext.getHost())
				|| oldPort == null && messageContext.getPort() != null
				|| oldPort != null && !oldPort.equals(messageContext.getPort());
		}

		private void logVerstuurProblemenOpgelost()
		{
			if (verstuurProblemen)
			{
				OpenHibernate5Session.withCommittedTransaction().run(() ->
					logService.logGebeurtenis(LogGebeurtenis.CERVIX_HL7V2_BERICHT_VERBINDING_HERSTELD, null,
						String.format("Lab %s: HL7v2 berichten worden weer succesvol verstuurd", labNaam),
						Bevolkingsonderzoek.CERVIX));
				verstuurProblemen = false;
			}
		}

		private void logVerstuurProblemen(Exception e)
		{
			if (!verstuurProblemen)
			{
				LOG.error(e.getMessage(), e);
				OpenHibernate5Session.withCommittedTransaction().run(() ->
					logService.logGebeurtenis(LogGebeurtenis.CERVIX_HL7V2_BERICHT_VERSTUREN_MISLUKT, null,
						String.format("Lab %s: Onbekende fout bij het ophalen van het te versturen HL7v2 bericht.", labNaam),
						Bevolkingsonderzoek.CERVIX));
				verstuurProblemen = true;
			}
		}

		private void logVerstuurProblemen(Message message, CervixHL7v24HpvOrderTriggerDto hl7BerichtTriggerDto, Exception e)
		{
			if (!verstuurProblemen)
			{
				String logMelding = String.format("Lab %s: HL7v2 bericht (id:%s) (%s) kon niet worden verstuurd. Reden: %s", labNaam, message.getId(), message.getType(),
					e.getMessage());
				LOG.error(logMelding, e);
				OpenHibernate5Session.withCommittedTransaction().run(() ->
					logService.logGebeurtenis(LogGebeurtenis.CERVIX_HL7V2_BERICHT_VERSTUREN_MISLUKT, getClient(getMonster(hl7BerichtTriggerDto)), logMelding,
						Bevolkingsonderzoek.CERVIX));
				verstuurProblemen = true;
			}
		}

		private void logQueueSizeProblemen(Long queueSize)
		{
			boolean oldQueueSizeWarningValue = queueSizeWarning;
			queueSizeWarning = queueSize > QUEUE_WARNING_THRESHOLD;
			if (oldQueueSizeWarningValue != queueSizeWarning)
			{
				if (queueSizeWarning)
				{
					LOG.warn("Lab {}: Queue size wordt te groot!", labNaam);
					OpenHibernate5Session.withCommittedTransaction().run(() ->
						logService.logGebeurtenis(LogGebeurtenis.CERVIX_HL7V2_BERICHT_QUEUE_ERG_GROOT, null,
							String.format("Lab %s: Er staan meer dan %d berichten in de queue", labNaam, QUEUE_WARNING_THRESHOLD),
							Bevolkingsonderzoek.CERVIX));
				}
				else
				{
					LOG.info("Lab {}: Queue size wordt weer klein genoeg", labNaam);
					OpenHibernate5Session.withCommittedTransaction().run(() ->
						logService.logGebeurtenis(LogGebeurtenis.CERVIX_HL7V2_BERICHT_QUEUE_NORMAAL, null,
							String.format("Lab %s: Het aantal berichten in de queue is weer normaal.", labNaam),
							Bevolkingsonderzoek.CERVIX));
				}
			}
		}

		private Client getClient(CervixMonster monster)
		{
			if (monster == null)
			{
				return null;
			}
			return monster.getUitnodiging().getScreeningRonde().getDossier().getClient();
		}

		private CervixMonster getMonster(CervixHL7v24HpvOrderTriggerDto triggerDto)
		{
			if (triggerDto == null)
			{
				return null;
			}
			var monster = hibernateService.get(triggerDto.getClazz(), triggerDto.getMonsterId());
			if (monster == null)
			{
				LOG.error("Monster met id {} is niet (meer) aanwezig", triggerDto.getMonsterId());
				return null;
			}
			hibernateService.reload(monster);
			monster = (CervixMonster) HibernateHelper.deproxy(monster);
			return monster;
		}

	}
}
