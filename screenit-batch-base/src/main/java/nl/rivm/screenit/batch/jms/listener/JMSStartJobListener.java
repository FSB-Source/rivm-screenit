package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.Serializable;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.Executors;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.BatchJobService;
import nl.rivm.screenit.model.batch.BatchJob;
import nl.rivm.screenit.model.enums.BatchApplicationType;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.service.MailService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobInstance;
import org.springframework.batch.core.JobParameter;
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

public class JMSStartJobListener implements SessionAwareMessageListener<ActiveMQObjectMessage>, ApplicationListener<ContextRefreshedEvent>
{
	private static final String JMS_MESSAGE_ID = "JMSMessageId";

	private static final Logger LOG = LoggerFactory.getLogger(JMSStartJobListener.class);

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

	private BatchApplicationType batchApplicationType;

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
		Executors.newSingleThreadExecutor().submit(new Runnable()
		{
			@Override
			public void run()
			{
				int i = 0;
				try
				{
					while (true)
					{
						if (i % 20 == 0) 
						{
							LOG.info("Heartbeat jobDequeueThread");
						}
						i++;
						JobType jobType = batchJobService.getHeadOfBatchJobQueue(batchApplicationType);
						if (jobType != null)
						{
							try
							{
								batchJobService.waitForJobLock(jobType);

								Map<String, Serializable> jobArgs = batchJobService.dequeueHead(jobType);
								JobParametersBuilder jobParametersBuilder = new JobParametersBuilder();
								overrideParametersWithSaved(jobParametersBuilder, jobArgs);

								Job job = jobLocator.getJob(jobType.name().toLowerCase());
								launcher.run(job, jobParametersBuilder.toJobParameters());
							}
							catch (Exception e)
							{
								String melding = "Onbekende fout tijdens starten/uitvoeren jobtype: " + jobType;
								LOG.error(melding, e);
								sendErrorEmail();
								throw new RuntimeException(melding, e);
							}
							finally
							{
								batchJobService.unlockJob(jobType);
							}
						}
						else
						{
							try
							{
								Thread.sleep(3000L);
							}
							catch (InterruptedException e)
							{
							}
						}
					}
				}
				catch (Exception e)
				{
					LOG.error("Fout in queue polling", e);
				}
			}
		});
	}

	private void overrideParametersWithSaved(JobParametersBuilder builder, Map<String, Serializable> jobArgs)
	{
		if (jobArgs != null)
		{
			for (Entry<String, Serializable> param : jobArgs.entrySet())
			{
				String key = param.getKey();
				Serializable value = param.getValue();
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
		Map<String, Serializable> jobParamsToSave = new HashMap<>();

		for (Entry<String, JobParameter> entry : jobParametersBuilder.toJobParameters().getParameters().entrySet())
		{
			String key = entry.getKey();
			JobParameter value = entry.getValue();
			if (JMS_MESSAGE_ID.equals(key) || key.startsWith(Constants.JOB_PARAMETER_PREFIX))
			{
				jobParamsToSave.put(key, (Serializable) value.getValue());
			}
		}
		return jobParamsToSave;
	}

	private JobParametersBuilder buildJobParameters(Message message, BatchJob batchJob) throws JMSException
	{
		JobParametersBuilder builder = new JobParametersBuilder();

		for (Entry<String, Object> entry : batchJob.getJobParameters().entrySet())
		{
			String key = entry.getKey();
			Object value = entry.getValue();
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
			BatchJob batchJob = (BatchJob) message.getObject();

			JobType jobType = batchJob.getJobType();
			boolean resume = batchJob.getJobParameters().containsKey("resume");

			String startResume = "start";
			if (resume)
			{
				startResume = "resume";
			}
			LOG.info("Request to " + startResume + " batch job " + jobType.name() + " by message " + message.getJMSMessageID());

			if (!resume)
			{
				JobParametersBuilder jobParametersBuilder = buildJobParameters(message, batchJob);
				Map<String, Serializable> jobParametersToSave = buildJobParametersToSave(jobParametersBuilder);
				batchJobService.enqueue(jobType, jobParametersToSave);
			}
			else
			{
				List<JobInstance> jobInstances = jobExplorer.getJobInstances(batchJob.getJobType().name().toLowerCase(), 0, 1);
				if (!jobInstances.isEmpty())
				{
					List<JobExecution> jobExecutions = jobExplorer.getJobExecutions(jobInstances.get(0));
					if (!jobExecutions.isEmpty())
					{
						for (JobExecution jobExecution : jobExecutions)
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
			String emailadressen = simplePreferenceService.getString(PreferenceKey.DASHBOARDEMAIL.name());
			if (emailadressen != null)
			{
				String subject = String.format("ScreenIT batchservice %s gefaald op %s", batchApplicationType, applicationEnvironment);
				String content = String.format("Er is een onbekende fout opgetreden in de batchservice van %s op %s. Neem contact op met de Topicus helpdesk om dit op te lossen.",
					batchApplicationType, applicationEnvironment);
				mailService.sendEmail(emailadressen, subject, content, MailService.MailPriority.HIGH);
			}
		}
		catch (Exception e)
		{
			LOG.error("Kon geen mail sturen: ", e);
		}
	}
}
