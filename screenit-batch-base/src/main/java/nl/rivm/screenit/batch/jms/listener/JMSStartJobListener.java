package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Executors;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.BatchJobService;
import nl.rivm.screenit.model.batch.BatchJob;
import nl.rivm.screenit.model.enums.BatchApplicationType;
import nl.rivm.screenit.model.enums.JobStartParameter;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.enums.MailPriority;
import nl.rivm.screenit.service.MailService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.JobParametersInvalidException;
import org.springframework.batch.core.configuration.JobLocator;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.launch.JobOperator;
import org.springframework.batch.core.launch.NoSuchJobException;
import org.springframework.batch.core.launch.NoSuchJobExecutionException;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.batch.core.repository.JobRestartException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.jms.listener.SessionAwareMessageListener;

@Slf4j
public class JMSStartJobListener implements SessionAwareMessageListener<ActiveMQObjectMessage>, ApplicationListener<ContextRefreshedEvent>
{
	private static final String JMS_MESSAGE_ID = "JMSMessageId";

	@Autowired
	private JobLocator jobLocator;

	@Autowired
	private JobLauncher launcher;

	@Autowired
	private JobExplorer jobExplorer;

	@Autowired
	private JobOperator jobOperator;

	@Autowired
	private MailService mailService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private String applicationEnvironment;

	@Autowired
	private BatchJobService batchJobService;

	private final BatchApplicationType batchApplicationType;

	public JMSStartJobListener(BatchApplicationType batchApplicationType)
	{
		this.batchApplicationType = batchApplicationType;
	}

	private boolean doInit = true;

	@Override
	public void onApplicationEvent(ContextRefreshedEvent contextRefreshedEvent)
	{
		if (doInit)
		{
			LOG.info("startJobDequeueThread");
			try
			{
				startJobDequeueThread();
			}
			catch (Exception e)
			{
				LOG.error("Fout bij opstarten", e);
			}
			finally
			{
				doInit = false;
			}

		}
	}

	private void startJobDequeueThread()
	{
		Executors.newSingleThreadExecutor().submit(() ->
		{
			var i = 0;
			var stop = false;
			while (!stop)
			{
				try
				{
					if (i % 20 == 0) 
					{
						LOG.info("Heartbeat jobDequeueThread");
					}
					i++;
					var jobType = batchJobService.getHeadOfBatchJobQueue(batchApplicationType);
					if (jobType != null)
					{
						voerJobUit(jobType);
					}
					else
					{
						Thread.sleep(3000L);
					}
				}
				catch (InterruptedException e)
				{
					LOG.error("InterruptException in queue polling", e);
					Thread.currentThread().interrupt();
					stop = true;
				}
				catch (Exception e)
				{
					LOG.error("Fout tijdens queue polling", e);
				}
			}
		});
	}

	private void voerJobUit(JobType jobType)
	{
		try
		{
			batchJobService.waitForJobLock(jobType);

			var jobArgs = batchJobService.dequeueHead(jobType);
			var jobParametersBuilder = new JobParametersBuilder();
			overrideParametersWithSaved(jobParametersBuilder, jobArgs);

			var job = jobLocator.getJob(jobType.name());
			launcher.run(job, jobParametersBuilder.toJobParameters());
		}
		catch (Exception e)
		{
			var melding = "Onbekende fout tijdens starten/uitvoeren jobtype: " + jobType;
			LOG.error(melding, e);
			sendErrorEmail();
			throw new IllegalStateException(melding, e);
		}
		finally
		{
			batchJobService.unlockJob(jobType);
		}
	}

	private void overrideParametersWithSaved(JobParametersBuilder builder, Map<String, Serializable> jobArgs)
	{
		if (jobArgs != null)
		{
			for (var param : jobArgs.entrySet())
			{
				var key = param.getKey();
				var value = param.getValue();
				if (value instanceof String)
				{
					builder.addString(key, (String) value);
				}
				else if (value instanceof Float || value instanceof Double)
				{
					builder.addDouble(key, ((Number) value).doubleValue());
				}
				else if (value instanceof Integer || value instanceof Long)
				{
					builder.addLong(key, ((Number) value).longValue());
				}
				else if (value instanceof Date)
				{
					builder.addDate(key, (Date) value);
				}
				else
				{
					LOG.warn("JobDataMap contains values which are not job parameters (ignoring). " + key + "=" + value);
				}
			}
		}
	}

	private Map<String, Serializable> buildJobParametersToSave(JobParametersBuilder jobParametersBuilder)
	{
		var jobParamsToSave = new HashMap<String, Serializable>();

		for (var entry : jobParametersBuilder.toJobParameters().getParameters().entrySet())
		{
			var key = entry.getKey();
			var value = entry.getValue();
			if (JMS_MESSAGE_ID.equals(key) || Arrays.stream(JobStartParameter.values()).anyMatch(p -> p.name().equals(key)))
			{
				jobParamsToSave.put(key, (Serializable) value.getValue());
			}
		}
		return jobParamsToSave;
	}

	private JobParametersBuilder buildJobParameters(Message message, BatchJob batchJob) throws JMSException
	{
		var builder = new JobParametersBuilder();

		for (var entry : batchJob.getJobParameters().entrySet())
		{
			var key = entry.getKey();
			var value = entry.getValue();
			if (value instanceof String && !key.equals(QuartzJobHelper.DEQUEUED_BATCH_JOB_TRIGGER))
			{
				builder.addString(key, (String) value);
			}
			else if (value instanceof Float || value instanceof Double)
			{
				builder.addDouble(key, ((Number) value).doubleValue());
			}
			else if (value instanceof Integer || value instanceof Long)
			{
				builder.addLong(key, ((Number) value).longValue());
			}
			else if (value instanceof Date)
			{
				builder.addDate(key, (Date) value);
			}
			else if (value instanceof Boolean)
			{
				builder.addString(key, value.toString());
			}
			else
			{
				LOG.warn("JobDataMap contains values which are not job parameters (ignoring). " + key + "=" + value);
			}
		}
		builder.addString(JMS_MESSAGE_ID, message.getJMSMessageID());
		return builder;
	}

	@Override
	public void onMessage(ActiveMQObjectMessage message, Session session)
	{
		try
		{
			var batchJob = (BatchJob) message.getObject();

			var jobType = batchJob.getJobType();
			var resume = batchJob.getJobParameters().containsKey("resume");

			var startResume = "start";
			if (resume)
			{
				startResume = "resume";
			}
			LOG.info("Request to " + startResume + " batch job " + jobType.name() + " by message " + message.getJMSMessageID());

			if (!resume)
			{
				var jobParametersBuilder = buildJobParameters(message, batchJob);
				var jobParametersToSave = buildJobParametersToSave(jobParametersBuilder);
				batchJobService.enqueue(jobType, jobParametersToSave);
			}
			else
			{
				var jobInstances = jobExplorer.getJobInstances(batchJob.getJobType().name().toLowerCase(), 0, 1);
				if (!jobInstances.isEmpty())
				{
					var jobExecutions = jobExplorer.getJobExecutions(jobInstances.get(0));
					if (!jobExecutions.isEmpty())
					{
						for (var jobExecution : jobExecutions)
						{
							if (ExitStatus.FAILED.equals(jobExecution.getExitStatus()))
							{
								jobOperator.restart(jobExecution.getId());
								break;
							}
						}
					}
				}
			}
		}
		catch (JMSException e)
		{
			LOG.error("Could not deserialize object from message", e);
		}
		catch (JobRestartException | JobInstanceAlreadyCompleteException | JobParametersInvalidException e)
		{
			LOG.error("Job could not be started", e);
		}
		catch (NoSuchJobException e)
		{
			LOG.error("Job of specified type could not be found", e);
		}
		catch (NoSuchJobExecutionException e)
		{
			LOG.error("Job could not be resumed", e);
		}
	}

	private void sendErrorEmail()
	{
		try
		{
			var emailadressen = simplePreferenceService.getString(PreferenceKey.DASHBOARDEMAIL.name());
			if (emailadressen != null)
			{
				var subject = String.format("ScreenIT batchservice %s gefaald op %s", batchApplicationType, applicationEnvironment);
				var content = String.format("Er is een onbekende fout opgetreden in de batchservice van %s op %s. Neem contact op met de Topicus helpdesk om dit op te lossen.",
					batchApplicationType, applicationEnvironment);
				mailService.queueMail(emailadressen, subject, content, MailPriority.HIGH);
			}
		}
		catch (Exception e)
		{
			LOG.error("Kon geen mail sturen: ", e);
		}
	}
}
