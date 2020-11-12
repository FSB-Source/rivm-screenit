package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.Serializable;
import java.util.Set;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

import nl.rivm.screenit.model.batch.BatchServerStatus;
import nl.rivm.screenit.model.batch.GetBatchStatusRequest;
import nl.rivm.screenit.model.batch.Job;
import nl.rivm.screenit.model.batch.JobInstance;
import nl.rivm.screenit.model.batch.StopAllBatchJobsRequest;
import nl.rivm.screenit.model.helper.ActiveMQHelper;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.launch.JobExecutionNotRunningException;
import org.springframework.batch.core.launch.JobOperator;
import org.springframework.batch.core.launch.NoSuchJobException;
import org.springframework.batch.core.launch.NoSuchJobExecutionException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jms.JmsException;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.core.MessageCreator;
import org.springframework.jms.listener.SessionAwareMessageListener;

public class JMSJobStatusListener implements SessionAwareMessageListener<ActiveMQObjectMessage>
{
	private static final Logger LOG = LoggerFactory.getLogger(JMSJobStatusListener.class);

	@Autowired
	private JobOperator jobOperator;

	@Autowired
	private JmsTemplate jmsTemplate;

	@Autowired
	private Scheduler scheduler;

	@Override
	public void onMessage(ActiveMQObjectMessage message, Session session) throws JMSException
	{
		try
		{
			LOG.info("Reply queue: " + message.getJMSReplyTo());
			Serializable object = message.getObject();
			if (object instanceof GetBatchStatusRequest)
			{
				jmsTemplate.send(message.getJMSReplyTo(), new MessageCreator()
				{
					
					@Override
					public Message createMessage(Session session) throws JMSException
					{
						Set<String> jobs = jobOperator.getJobNames();
						BatchServerStatus serverStatus = new BatchServerStatus();

						try
						{
							serverStatus.setInstanceName(scheduler.getSchedulerInstanceId());
						}
						catch (SchedulerException e)
						{
							LOG.error("Could not resolve scheduler name", e);
						}

						for (String jobName : jobs)
						{
							Job job = new Job();
							job.setJobName(jobName);
							serverStatus.getJobs().add(job);

							try
							{
								Set<Long> executionIDs = jobOperator.getRunningExecutions(jobName);
								for (Long executionID : executionIDs)
								{
									JobInstance jobInstance = new JobInstance();
									job.getInstances().add(jobInstance);
									jobInstance.setSamenvatting(jobOperator.getSummary(executionID));
								}
							}
							catch (NoSuchJobException | NoSuchJobExecutionException e)
							{
								LOG.error("Job not found", e);
							}

						}
						return ActiveMQHelper.getActiveMqObjectMessage(serverStatus);
					}
				});
			}
			else if (object instanceof StopAllBatchJobsRequest)
			{
				jmsTemplate.send(message.getJMSReplyTo(), new MessageCreator()
				{
					
					@Override
					public Message createMessage(Session session) throws JMSException
					{
						Set<String> jobs = jobOperator.getJobNames();

						for (String jobName : jobs)
						{
							LOG.info("Stop job " + jobName);
							try
							{
								Set<Long> executionIDs = jobOperator.getRunningExecutions(jobName);
								for (Long executionID : executionIDs)
								{
									jobOperator.stop(executionID);
								}
							}
							catch (NoSuchJobException | NoSuchJobExecutionException | JobExecutionNotRunningException e)
							{
								LOG.error("Fout bij stoppen job " + jobName, e);
							}

						}
						return ActiveMQHelper.getActiveMqObjectMessage(true);
					}
				});
			}

		}
		catch (JmsException | JMSException e)
		{
			LOG.error("Error replying to message", e);
		}
	}

}
