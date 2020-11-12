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

import nl.rivm.screenit.batch.quartz.JobLauncherDetails;
import nl.rivm.screenit.model.enums.JobType;

import org.quartz.JobBuilder;
import org.quartz.JobDetail;

public class QuartzJobHelper
{
	
	public static final String JOB_READY = "jobReady";

	public static final String JOB_NAME = "jobName";

	public static final String DEQUEUED_BATCH_JOB_TRIGGER = "dequeued";

	private QuartzJobHelper()
	{
	}

	public static JobDetail createNewJobDetail(JobType jobType)
	{
		JobBuilder jobBuilder = JobBuilder.newJob(JobLauncherDetails.class);
		jobBuilder = jobBuilder.withIdentity(jobType.name());
		jobBuilder = jobBuilder.usingJobData(JOB_NAME, jobType.name());
		jobBuilder = jobBuilder.storeDurably();
		return jobBuilder.build();
	}

	public static JobDetail createReadyJobDetail()
	{
		JobBuilder jobBuilder = JobBuilder.newJob(JobLauncherDetails.class);
		jobBuilder = jobBuilder.withIdentity(JOB_READY);
		jobBuilder = jobBuilder.storeDurably();
		return jobBuilder.build();
	}
}
