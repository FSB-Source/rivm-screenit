package nl.rivm.screenit.batch.jobs.cervix.verlatedeelnamecovid;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.cervix.CervixLabPartitioner;
import nl.rivm.screenit.batch.jobs.cervix.verlatedeelnamecovid.step.CervixVerlateDeelnameCovidDecider;
import nl.rivm.screenit.batch.jobs.cervix.verlatedeelnamecovid.step.CervixVerlateDeelnameCovidReader;
import nl.rivm.screenit.batch.jobs.cervix.verlatedeelnamecovid.step.CervixVerlateDeelnameCovidWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.partition.support.TaskExecutorPartitionHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

@Configuration
public class CervixVerlateDeelnameCovidJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job verlateDeelnameCovidJob(CervixVerlateDeelnameCovidJobListener listener, Step dummyStep, CervixVerlateDeelnameCovidDecider verlateDeelnameDecider,
		Step verlateDeelnameCovidPartitionerStep)
	{
		return jobBuilderFactory.get(JobType.CERVIX_VERLATE_DEELNAME_COVID19.name())
			.listener(listener)
			.start(dummyStep)
			.next(verlateDeelnameDecider)
			.on(ExitStatus.FAILED.getExitCode()).to(verlateDeelnameCovidPartitionerStep)
			.from(verlateDeelnameDecider)
			.on(ExitStatus.COMPLETED.getExitCode()).end()
			.end().build();
	}

	@Bean
	public Step verlateDeelnameCovidPartitionerStep(CervixLabPartitioner partitioner, Step verlateDeelnameCovidPerLabStep,
		TaskExecutorPartitionHandler verlateDeelnameCovidPartitionHandler)
	{
		return stepBuilderFactory.get("verlateDeelnameCovidPartitionerStep")
			.transactionManager(transactionManager)
			.partitioner("verlateDeelnameCovidPerLabStep", partitioner)
			.partitionHandler(verlateDeelnameCovidPartitionHandler)
			.step(verlateDeelnameCovidPerLabStep)
			.build();
	}

	@Bean
	public TaskExecutorPartitionHandler verlateDeelnameCovidPartitionHandler(TaskExecutor taskExecutor, Step verlateDeelnameCovidPerLabStep)
	{
		var partitionHandler = new TaskExecutorPartitionHandler();
		partitionHandler.setTaskExecutor(taskExecutor);
		partitionHandler.setGridSize(10);
		partitionHandler.setStep(verlateDeelnameCovidPerLabStep);
		return partitionHandler;
	}

	@Bean
	public Step verlateDeelnameCovidPerLabStep(CervixVerlateDeelnameCovidReader reader, CervixVerlateDeelnameCovidWriter writer)
	{
		return stepBuilderFactory.get("verlateDeelnameCovidPerLabStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(1)
			.reader(reader)
			.writer(writer)
			.build();
	}
}
