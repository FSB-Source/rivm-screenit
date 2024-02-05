package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import javax.jms.JMSException;
import javax.jms.Session;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.batch.BatchServerStatus;
import nl.rivm.screenit.model.batch.GetBatchStatusRequest;
import nl.rivm.screenit.model.batch.Job;
import nl.rivm.screenit.model.batch.JobInstance;
import nl.rivm.screenit.model.batch.StopAllBatchJobsRequest;
import nl.rivm.screenit.model.helper.ActiveMQHelper;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.springframework.batch.core.launch.JobExecutionNotRunningException;
import org.springframework.batch.core.launch.JobOperator;
import org.springframework.batch.core.launch.NoSuchJobException;
import org.springframework.batch.core.launch.NoSuchJobExecutionException;
import org.springframework.jms.JmsException;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.listener.SessionAwareMessageListener;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class JMSJobStatusListener implements SessionAwareMessageListener<ActiveMQObjectMessage>
{

	private final JobOperator jobOperator;

	private final JmsTemplate jmsTemplate;

	private final Scheduler scheduler;

	public JMSJobStatusListener(JobOperator jobOperator, JmsTemplate jmsTemplate, Scheduler scheduler)
	{
		this.jobOperator = jobOperator;
		this.jmsTemplate = jmsTemplate;
		this.scheduler = scheduler;
	}

	@Override
	public void onMessage(ActiveMQObjectMessage message, Session session)
	{
		try
		{
			LOG.info("Reply queue: " + message.getJMSReplyTo());
			var object = message.getObject();
			if (object instanceof GetBatchStatusRequest)
			{
				jmsTemplate.send(message.getJMSReplyTo(), session1 ->
				{
					var jobs = jobOperator.getJobNames();
					var serverStatus = new BatchServerStatus();

					try
					{
						serverStatus.setInstanceName(scheduler.getSchedulerInstanceId());
					}
					catch (SchedulerException e)
					{
						LOG.error("Could not resolve scheduler name", e);
					}

					for (var jobName : jobs)
					{
						var job = new Job();
						job.setJobName(jobName);
						serverStatus.getJobs().add(job);

						try
						{
							var executionIDs = jobOperator.getRunningExecutions(jobName);
							for (var executionID : executionIDs)
							{
								var jobInstance = new JobInstance();
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
				});
			}
			else if (object instanceof StopAllBatchJobsRequest)
			{
				jmsTemplate.send(message.getJMSReplyTo(), session12 ->
				{
					var jobs = jobOperator.getJobNames();

					for (var jobName : jobs)
					{
						LOG.info("Stop job " + jobName);
						try
						{
							var executionIDs = jobOperator.getRunningExecutions(jobName);
							for (var executionID : executionIDs)
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
				});
			}

		}
		catch (JmsException | JMSException e)
		{
			LOG.error("Error replying to message", e);
		}
	}

}
