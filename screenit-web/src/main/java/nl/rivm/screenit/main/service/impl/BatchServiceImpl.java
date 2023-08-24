
package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import javax.jms.DeliveryMode;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.ObjectMessage;
import javax.jms.QueueBrowser;
import javax.jms.Session;
import javax.jms.TemporaryQueue;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.AsyncMessageReceiver;
import nl.rivm.screenit.main.service.BatchService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.batch.AddTriggerRequest;
import nl.rivm.screenit.model.batch.AddTriggerResponse;
import nl.rivm.screenit.model.batch.BatchQueue;
import nl.rivm.screenit.model.batch.BatchServerStatus;
import nl.rivm.screenit.model.batch.GetBatchStatusRequest;
import nl.rivm.screenit.model.batch.GetTriggersRequest;
import nl.rivm.screenit.model.batch.GetTriggersResponse;
import nl.rivm.screenit.model.batch.RemoveTriggerRequest;
import nl.rivm.screenit.model.batch.RemoveTriggerResponse;
import nl.rivm.screenit.model.batch.StopAllBatchJobsRequest;
import nl.rivm.screenit.model.batch.Trigger;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.helper.ActiveMQHelper;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jms.UncategorizedJmsException;
import org.springframework.jms.core.BrowserCallback;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.core.MessageCreator;
import org.springframework.jms.core.SessionCallback;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class BatchServiceImpl implements BatchService
{

	@Autowired
	private JmsTemplate jmsTemplate;

	@Autowired
	private AsyncMessageReceiver asyncMessageReceiver;

	@Autowired
	@Qualifier(value = "batchServerStatusDestination")
	private Destination batchServerStatusDestination;

	@Autowired
	@Qualifier(value = "quartzDestination")
	private Destination quartzDestination;

	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Override
	public List<BatchServerStatus> getBatchServerStatus() throws UncategorizedJmsException
	{
		LOG.trace("getBatchServerStatus");

		return jmsTemplate.execute(session ->
		{
			final List<BatchServerStatus> batchServerStatussen = new ArrayList<>();
			var temporaryQueue = session.createTemporaryQueue();
			try
			{
				jmsTemplate.send(batchServerStatusDestination, new BatchMessageCreator(temporaryQueue)
				{
					@Override
					protected Message createMessageSpecifiek(Session session) throws JMSException
					{
						return ActiveMQHelper.getActiveMqObjectMessage(new GetBatchStatusRequest());
					}

				});

				Thread.sleep(2000);

				batchServerStatussen.addAll(jmsTemplate.browse(temporaryQueue, (s, browser) ->
				{
					LOG.trace("getBatchServerStatus: doInJms");
					List<BatchServerStatus> result = new ArrayList<>();
					Enumeration<Message> enumeration = browser.getEnumeration();
					while (enumeration.hasMoreElements())
					{
						Message message = enumeration.nextElement();
						if (message instanceof ObjectMessage)
						{
							ObjectMessage objectMessage = (ObjectMessage) message;
							if (objectMessage.getObject() instanceof BatchServerStatus)
							{
								result.add((BatchServerStatus) objectMessage.getObject());
							}
						}
					}
					return result;
				}));
			}
			catch (InterruptedException e)
			{
				LOG.error("Thread interrupted", e);
			}
			finally
			{
				temporaryQueue.delete();
			}
			return batchServerStatussen;
		});
	}

	@Override
	public void getStopAllBatchJobs() throws UncategorizedJmsException
	{
		LOG.trace("getStopAllBatchJobs");
		jmsTemplate.execute(new SessionCallback<Boolean>()
		{

			@Override
			public Boolean doInJms(Session session) throws JMSException
			{

				TemporaryQueue temporaryQueue = session.createTemporaryQueue();
				try
				{

					jmsTemplate.send(batchServerStatusDestination, new BatchMessageCreator(temporaryQueue)
					{
						@Override
						protected Message createMessageSpecifiek(Session session) throws JMSException
						{
							return ActiveMQHelper.getActiveMqObjectMessage(new StopAllBatchJobsRequest());
						}

					});

					Thread.sleep(2000);

					return jmsTemplate.browse(temporaryQueue, new BrowserCallback<Boolean>()
					{

						@Override
						public Boolean doInJms(Session session, QueueBrowser browser) throws JMSException
						{
							Boolean result = null;
							LOG.trace("getStopAllBatchJobs: doInJms");
							Enumeration<Message> enumeration = browser.getEnumeration();
							while (enumeration.hasMoreElements())
							{
								Message message = enumeration.nextElement();
								if (message instanceof ObjectMessage)
								{
									ObjectMessage objectMessage = (ObjectMessage) message;
									if (objectMessage.getObject() instanceof BatchServerStatus)
									{
										result = (Boolean) objectMessage.getObject();
										break;
									}
								}
							}
							return result;
						}
					});

				}
				catch (InterruptedException e)
				{
					LOG.error("Thread interrupted", e);
				}
				finally
				{
					temporaryQueue.delete();
				}
				return false;
			}
		});

	}

	@Override
	public List<Trigger> getScheduledTriggers() throws UncategorizedJmsException
	{
		List<Trigger> result = jmsTemplate.execute(new SessionCallback<List<Trigger>>()
		{
			@Override
			public List<Trigger> doInJms(Session session) throws JMSException
			{
				TemporaryQueue temporaryQueue = session.createTemporaryQueue();
				Future<Message> message = asyncMessageReceiver.receiveMessage(temporaryQueue, 10000L);
				try
				{
					jmsTemplate.send(quartzDestination, new BatchMessageCreator(temporaryQueue)
					{
						@Override
						protected Message createMessageSpecifiek(Session session) throws JMSException
						{
							LOG.trace("getScheduledTriggers: createMessage");
							return ActiveMQHelper.getActiveMqObjectMessage(new GetTriggersRequest());
						}
					});

					Thread.sleep(2000);

					Message receivedMessage = message.get();
					if (receivedMessage instanceof ObjectMessage)
					{
						ObjectMessage objectMessage = (ObjectMessage) receivedMessage;
						if (objectMessage.getObject() instanceof GetTriggersResponse)
						{
							GetTriggersResponse getTriggersResponse = (GetTriggersResponse) objectMessage.getObject();
							return getTriggersResponse.getTriggers();
						}
					}
					return null;
				}
				catch (InterruptedException e)
				{
					LOG.error("Thread interrupted", e);
				}
				catch (ExecutionException e)
				{

				}
				return null;
			}
		});
		return result;
	}

	@Override
	public Date addTrigger(final Trigger trigger, InstellingGebruiker instellingGebruiker)
	{
		Date result = jmsTemplate.execute(new SessionCallback<Date>()
		{
			@Override
			public Date doInJms(Session session) throws JMSException
			{
				TemporaryQueue temporaryQueue = session.createTemporaryQueue();
				LOG.trace("temp queue: " + temporaryQueue.getQueueName());
				Future<Message> message = asyncMessageReceiver.receiveMessage(temporaryQueue, 10000L);

				try
				{

					Thread.sleep(1000);
				}
				catch (InterruptedException e1)
				{
					LOG.error("Thread interrupted", e1);
				}

				jmsTemplate.send(quartzDestination, new BatchMessageCreator(temporaryQueue)
				{
					@Override
					protected Message createMessageSpecifiek(Session session) throws JMSException
					{
						LOG.trace("addTrigger: createMessage");
						AddTriggerRequest addTriggerRequest = new AddTriggerRequest();
						addTriggerRequest.setTrigger(trigger);

						return ActiveMQHelper.getActiveMqObjectMessage(addTriggerRequest);
					}
				});

				Message receivedMessage = null;
				try
				{
					receivedMessage = message.get();
				}
				catch (InterruptedException | ExecutionException e)
				{
					LOG.error("Error getting async result", e);
				}
				if (receivedMessage instanceof ObjectMessage)
				{
					ObjectMessage objectMessage = (ObjectMessage) receivedMessage;
					if (objectMessage.getObject() instanceof AddTriggerResponse)
					{
						AddTriggerResponse addTriggerResponse = (AddTriggerResponse) objectMessage.getObject();
						trigger.setTriggerNaam(addTriggerResponse.getTriggerName());
						return addTriggerResponse.getFirstScheduleDate();
					}

				}

				return null;
			}
		});

		if (instellingGebruiker != null)
		{
			LogEvent logEvent = new LogEvent();
			String logMelding = getTriggerForMelding(trigger);
			if (result == null)
			{
				logEvent.setLevel(Level.ERROR);
				logMelding = "Trigger kon niet aangemaakt worden => " + logMelding;
			}
			logEvent.setMelding(logMelding);
			logService.logGebeurtenis(LogGebeurtenis.BATCH_ADD_TRIGGER, logEvent, instellingGebruiker);
		}
		return result;
	}

	private String getTriggerForMelding(final Trigger trigger)
	{
		String triggerNaam = trigger.getTriggerNaam();
		String logMelding = "Job type: " + trigger.getJobType();
		if (StringUtils.isNotBlank(triggerNaam))
		{
			logMelding += ", Trigger key: " + triggerNaam;
		}
		logMelding += ", Cronexpressie: " + trigger.getCronExpressie();
		return logMelding;
	}

	@Override
	public Boolean removeTrigger(final String triggerNaam, InstellingGebruiker instellingGebruiker)
	{
		LOG.trace("removeTrigger");
		Boolean result = jmsTemplate.execute(new SessionCallback<Boolean>()
		{

			@Override
			public Boolean doInJms(Session session) throws JMSException
			{
				TemporaryQueue temporaryQueue = session.createTemporaryQueue();
				Future<Message> message = asyncMessageReceiver.receiveMessage(temporaryQueue, 10000L);

				try
				{

					Thread.sleep(1000);
				}
				catch (InterruptedException e1)
				{
					LOG.error("Thread interrupted", e1);
				}

				jmsTemplate.send(quartzDestination, new BatchMessageCreator(temporaryQueue)
				{
					@Override
					protected Message createMessageSpecifiek(Session session) throws JMSException
					{
						LOG.trace("removeTrigger: createMessage");
						RemoveTriggerRequest removeTriggerRequest = new RemoveTriggerRequest();
						removeTriggerRequest.setTriggerNaam(triggerNaam);

						return ActiveMQHelper.getActiveMqObjectMessage(removeTriggerRequest);
					}
				});

				Message receivedMessage = null;
				try
				{
					receivedMessage = message.get();
				}
				catch (InterruptedException | ExecutionException e)
				{
					LOG.error("Error getting async result", e);
				}

				if (receivedMessage instanceof ObjectMessage)
				{
					ObjectMessage objectMessage = (ObjectMessage) receivedMessage;
					if (objectMessage.getObject() instanceof RemoveTriggerResponse)
					{
						RemoveTriggerResponse removeTriggerResponse = (RemoveTriggerResponse) objectMessage.getObject();
						return removeTriggerResponse.getRemoveResult();
					}

				}

				return null;
			}

		});

		LogEvent logEvent = new LogEvent();
		String logMelding = "Trigger key: " + triggerNaam;
		if (!Boolean.TRUE.equals(result))
		{
			logEvent.setLevel(Level.ERROR);
			logMelding = "Trigger kon niet verwijderd worden => " + logMelding;
		}
		logEvent.setMelding(logMelding);
		logService.logGebeurtenis(LogGebeurtenis.BATCH_REMOVE_TRIGGER, logEvent, instellingGebruiker);

		return result;
	}

	@Override
	public List<JobType> getBatchQueue()
	{
		List<?> list = hibernateService.getHibernateSession().createSQLQuery(BatchQueue.SQL_GET_QUEUE).list();
		List<JobType> jobTypes = new ArrayList<>();
		for (Object object : list)
		{
			jobTypes.add(JobType.valueOf(object.toString()));
		}
		return jobTypes;
	}

	private class BatchMessageCreator implements MessageCreator
	{
		private final TemporaryQueue replyQueue;

		public BatchMessageCreator(TemporaryQueue replyQueue)
		{
			this.replyQueue = replyQueue;
		}

		@Override
		public final Message createMessage(Session session) throws JMSException
		{
			Message message = createMessageSpecifiek(session);
			message.setJMSReplyTo(replyQueue);
			message.setJMSDeliveryMode(DeliveryMode.NON_PERSISTENT);
			return message;
		}

		protected Message createMessageSpecifiek(Session session) throws JMSException
		{
			Message message = session.createMessage();
			return message;
		}
	}

}
