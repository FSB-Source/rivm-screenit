
package nl.rivm.screenit.batch.quartz;

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

import java.util.Date;
import java.util.Map;
import java.util.Map.Entry;

import nl.rivm.screenit.batch.jms.listener.QuartzJobHelper;
import nl.rivm.screenit.model.batch.BatchJob;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.service.JobService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.lang3.StringUtils;
import org.quartz.JobExecutionContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.quartz.QuartzJobBean;

public class JobLauncherDetails extends QuartzJobBean
{
	
	private static final Logger LOG = LoggerFactory.getLogger(JobLauncherDetails.class);

	@Override
	protected void executeInternal(JobExecutionContext context)
	{
		Map<String, Object> jobDataMap = context.getMergedJobDataMap();
		String jobTypeString = (String) jobDataMap.get(QuartzJobHelper.JOB_NAME);
		JobType jobType = null;
		BatchJob batchJob = new BatchJob();
		if (StringUtils.isBlank(jobTypeString))
		{
			throw new IllegalStateException("Job type not found: " + jobTypeString);
		}
		jobType = JobType.valueOf(jobTypeString);
		if (jobType == null)
		{
			throw new IllegalStateException("Job type not found: " + jobTypeString);
		}
		LOG.info("Quartz trigger firing with Spring Batch jobType=" + jobType.name());

		batchJob.setJobType(jobType);

		for (Entry<String, Object> entry : jobDataMap.entrySet())
		{
			String key = entry.getKey();
			Object value = entry.getValue();
			if (value instanceof String && !key.equals(QuartzJobHelper.JOB_NAME))
			{
				batchJob.getJobParameters().put(key, value);
			}
			else if (value instanceof Float || value instanceof Double)
			{
				batchJob.getJobParameters().put(key, value);
			}
			else if (value instanceof Integer || value instanceof Long)
			{
				batchJob.getJobParameters().put(key, value);
			}
			else if (value instanceof Date)
			{
				batchJob.getJobParameters().put(key, value);
			}
			else
			{
				LOG.debug("JobDataMap contains values which are not job parameters (ignoring).");
			}
		}

		SpringBeanProvider.getInstance().getBean(JobService.class).startJob(batchJob, null);
	}
}
