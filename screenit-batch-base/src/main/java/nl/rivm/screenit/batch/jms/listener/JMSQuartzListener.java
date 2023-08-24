package nl.rivm.screenit.batch.jms.listener;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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
import java.util.List;

import javax.jms.Session;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.batch.AddTriggerRequest;
import nl.rivm.screenit.model.batch.AddTriggerResponse;
import nl.rivm.screenit.model.batch.GetTriggersRequest;
import nl.rivm.screenit.model.batch.GetTriggersResponse;
import nl.rivm.screenit.model.batch.RemoveTriggerRequest;
import nl.rivm.screenit.model.batch.RemoveTriggerResponse;
import nl.rivm.screenit.model.batch.Trigger;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.helper.ActiveMQHelper;

import org.apache.activemq.command.ActiveMQObjectMessage;
import org.jgroups.util.UUID;
import org.quartz.CronScheduleBuilder;
import org.quartz.CronTrigger;
import org.quartz.JobDetail;
import org.quartz.JobKey;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.TriggerKey;
import org.quartz.spi.MutableTrigger;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.listener.SessionAwareMessageListener;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class JMSQuartzListener implements SessionAwareMessageListener<ActiveMQObjectMessage>
{
	private final Scheduler scheduler;

	private final JmsTemplate jmsTemplate;

	@Override
	public void onMessage(ActiveMQObjectMessage message, Session session)
	{
		try
		{
			Object object = message.getObject();
			LOG.info("Reply queue: " + message.getJMSReplyTo());
			if (object instanceof AddTriggerRequest)
			{
				AddTriggerRequest addTriggerRequest = (AddTriggerRequest) object;

				try
				{
					Trigger trigger = addTriggerRequest.getTrigger();
					final AddTriggerResponse addTriggerResponse = new AddTriggerResponse();
					if (trigger.getJobType() != null)
					{
						JobDetail jobDetail = scheduler.getJobDetail(new JobKey(trigger.getJobType().name()));

						boolean newJob = false;
						if (jobDetail == null)
						{
							jobDetail = QuartzJobHelper.createNewJobDetail(trigger.getJobType());
							newJob = true;
						}

						CronScheduleBuilder cronScheduleBuilder = CronScheduleBuilder.cronSchedule(trigger.getCronExpressie());

						TriggerKey key = new TriggerKey(UUID.randomUUID().toString());
						MutableTrigger mutableTrigger = cronScheduleBuilder.build();
						mutableTrigger.setKey(key);

						Date firstScheduleDate;
						if (newJob)
						{
							firstScheduleDate = scheduler.scheduleJob(jobDetail, mutableTrigger);
						}
						else
						{
							mutableTrigger.setJobKey(jobDetail.getKey());
							firstScheduleDate = scheduler.scheduleJob(mutableTrigger);
						}

						addTriggerResponse.setFirstScheduleDate(firstScheduleDate);
						addTriggerResponse.setTriggerName(key.getName());
					}
					else
					{
						addTriggerResponse.setTriggerName(trigger.getTriggerNaam());
					}
					jmsTemplate.send(message.getJMSReplyTo(), session13 -> ActiveMQHelper.getActiveMqObjectMessage(addTriggerResponse));
				}
				catch (SchedulerException e)
				{
					LOG.error("Error creating trigger", e);
				}
			}
			else if (object instanceof GetTriggersRequest)
			{
				try
				{
					final List<Trigger> triggers = new ArrayList<>();
					for (TriggerKey triggerKey : scheduler.getTriggerKeys(null))
					{
						org.quartz.Trigger quartzTrigger = scheduler.getTrigger(triggerKey);
						JobDetail jobDetail = scheduler.getJobDetail(quartzTrigger.getJobKey());
						if (quartzTrigger instanceof CronTrigger)
						{
							CronTrigger cronTrigger = (CronTrigger) quartzTrigger;
							Trigger trigger = new Trigger();
							trigger.setJobType(JobType.valueOf(jobDetail.getJobDataMap().getString(QuartzJobHelper.JOB_NAME)));
							trigger.setCronExpressie(cronTrigger.getCronExpression());
							trigger.setTriggerNaam(cronTrigger.getKey().getName());
							trigger.setNextFireTime(cronTrigger.getNextFireTime());
							triggers.add(trigger);
						}
					}

					jmsTemplate.send(message.getJMSReplyTo(), session12 ->
					{
						GetTriggersResponse getTriggersResponse = new GetTriggersResponse();
						getTriggersResponse.setTriggers(triggers);
						return ActiveMQHelper.getActiveMqObjectMessage(getTriggersResponse);
					});

				}
				catch (SchedulerException e)
				{
					LOG.error("Error creating trigger", e);
				}
			}
			else if (object instanceof RemoveTriggerRequest)
			{
				try
				{
					RemoveTriggerRequest removeTriggerRequest = (RemoveTriggerRequest) object;
					org.quartz.Trigger trigger = scheduler.getTrigger(new TriggerKey(removeTriggerRequest.getTriggerNaam()));
					final Boolean result = scheduler.unscheduleJob(trigger.getKey());

					jmsTemplate.send(message.getJMSReplyTo(), session1 ->
					{
						RemoveTriggerResponse removeTriggerResponse = new RemoveTriggerResponse();
						removeTriggerResponse.setRemoveResult(result);
						return ActiveMQHelper.getActiveMqObjectMessage(removeTriggerResponse);
					});
				}
				catch (SchedulerException e)
				{
					LOG.error("Error removing trigger", e);
				}
			}
		}
		catch (Exception e)
		{

			LOG.error("Error creating trigger", e);
		}
	}
}
