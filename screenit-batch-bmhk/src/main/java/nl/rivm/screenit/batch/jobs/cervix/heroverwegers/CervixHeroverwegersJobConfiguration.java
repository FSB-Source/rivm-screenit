package nl.rivm.screenit.batch.jobs.cervix.heroverwegers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.cervix.CervixLabPartitioner;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.partition.support.TaskExecutorPartitionHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

@Configuration
public class CervixHeroverwegersJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job heroverwegersJob(CervixHeroverwegersListener listener, Step heroverwegersPartitionerStep)
	{
		return jobBuilderFactory.get(JobType.CERVIX_HEROVERWEGERS.name())
			.listener(listener)
			.start(heroverwegersPartitionerStep)
			.build();
	}

	@Bean
	public Step heroverwegersPartitionerStep(CervixLabPartitioner partitioner, TaskExecutorPartitionHandler heroverwegersPartitionHandler, Step heroverwegersStep)
	{
		return stepBuilderFactory.get("heroverwegersPartitionerStep")
			.transactionManager(transactionManager)
			.partitioner("heroverwegersStep", partitioner)
			.partitionHandler(heroverwegersPartitionHandler)
			.step(heroverwegersStep)
			.build();
	}

	@Bean
	public TaskExecutorPartitionHandler heroverwegersPartitionHandler(TaskExecutor taskExecutor, Step heroverwegersStep)
	{
		var partitionHandler = new TaskExecutorPartitionHandler();
		partitionHandler.setTaskExecutor(taskExecutor);
		partitionHandler.setStep(heroverwegersStep);
		partitionHandler.setGridSize(10);
		return partitionHandler;
	}

	@Bean
	public Step heroverwegersStep(CervixHeroverwegersReader reader, CervixHeroverwegersWriter writer)
	{
		return stepBuilderFactory.get("heroverwegersStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
