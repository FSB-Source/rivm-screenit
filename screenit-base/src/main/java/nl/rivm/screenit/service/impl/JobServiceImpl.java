
package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.batch.BatchJob;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.helper.ActiveMQHelper;
import nl.rivm.screenit.service.JobService;
import nl.rivm.screenit.service.LogService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jms.core.JmsTemplate;
import org.springframework.jms.core.MessageCreator;
import org.springframework.stereotype.Service;

@Service
public class JobServiceImpl implements JobService
{

	private static final Logger LOG = LoggerFactory.getLogger(JobServiceImpl.class);

	@Autowired
	private JmsTemplate jmsTemplate;

	@Autowired
	@Qualifier(value = "generalisJobsDestination")
	private Destination generalisJobsDestination;

	@Autowired
	@Qualifier(value = "colonJobsDestination")
	private Destination colonJobsDestination;

	@Autowired
	@Qualifier(value = "cervixJobsDestination")
	private Destination cervixJobsDestination;

	@Autowired
	@Qualifier(value = "mammaJobsDestination")
	private Destination mammaJobsDestination;

	@Autowired
	private LogService logService;

	@Override
	public String startJob(JobType jobType, InstellingGebruiker instellingGebruiker)
	{
		BatchJob batchJob = new BatchJob();
		batchJob.setJobType(jobType);
		return startJob(batchJob, instellingGebruiker);
	}

	@Override
	public String startJob(final BatchJob batchJob, InstellingGebruiker instellingGebruiker)
	{
		MessageStoringMessageCreator messageStoringMessageCreator = new MessageStoringMessageCreator(batchJob);

		switch (batchJob.getJobType().getBatchApplicationType())
		{
		case CERVIX:
			jmsTemplate.send(cervixJobsDestination, messageStoringMessageCreator);
			break;
		case COLON:
			jmsTemplate.send(colonJobsDestination, messageStoringMessageCreator);
			break;
		case MAMMA:
			jmsTemplate.send(mammaJobsDestination, messageStoringMessageCreator);
			break;
		case GENERALIS:
			jmsTemplate.send(generalisJobsDestination, messageStoringMessageCreator);
			break;
		default:
			break;
		}

		try
		{

			String jmsMessageID = messageStoringMessageCreator.getMessage().getJMSMessageID();
			String infoJobStartParameter = batchJob.getJobParameters().toString();
			if (instellingGebruiker != null)
			{

				logService.logGebeurtenis(LogGebeurtenis.BATCH_START, instellingGebruiker, batchJob.getJobType() + ": " + jmsMessageID + " params: " + infoJobStartParameter);
			}
			return jmsMessageID;
		}
		catch (JMSException e)
		{
			LOG.error("Could not retrieve Message id", e);
			return null;
		}
	}

	@Override
	public void resumeClientSelectie()
	{
		BatchJob batchJob = new BatchJob();
		batchJob.setJobType(JobType.CLIENT_SELECTIE);
		batchJob.getJobParameters().put("resume", true);
		MessageStoringMessageCreator messageStoringMessageCreator = new MessageStoringMessageCreator(batchJob);

		jmsTemplate.send(colonJobsDestination, messageStoringMessageCreator);
	}

	private class MessageStoringMessageCreator implements MessageCreator
	{

		private final BatchJob batchJob;

		private Message message;

		public MessageStoringMessageCreator(BatchJob batchJob)
		{
			super();
			this.batchJob = batchJob;
		}

		@Override
		public Message createMessage(Session session) throws JMSException
		{
			message = ActiveMQHelper.getActiveMqObjectMessage(batchJob);
			return message;
		}

		public Message getMessage()
		{
			return message;
		}
	}
}
