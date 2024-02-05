package nl.rivm.screenit.batch.jobs.colon.brieven;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.colon.brieven.cleanupstep.ColonBriefCleanupReader;
import nl.rivm.screenit.batch.jobs.colon.brieven.cleanupstep.ColonBriefCleanupWriter;
import nl.rivm.screenit.batch.jobs.colon.brieven.controle.ColonBrievenControleReader;
import nl.rivm.screenit.batch.jobs.colon.brieven.controle.ColonBrievenControleWriter;
import nl.rivm.screenit.batch.jobs.colon.brieven.genererenstep.ColonBrievenGenererenPartitioner;
import nl.rivm.screenit.batch.jobs.colon.brieven.genererenstep.ColonBrievenGenererenProcessor;
import nl.rivm.screenit.batch.jobs.colon.brieven.genererenstep.ColonBrievenGenererenReader;
import nl.rivm.screenit.batch.jobs.colon.brieven.genererenstep.ColonBrievenGenererenWriter;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.partition.support.TaskExecutorPartitionHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

@Configuration
public class ColonBrievenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job brievenJob(ColonBrievenListener listener, Step brievenCleanupStep, Step brievenGenererenPartitionerStep, Step brievenControleStep)
	{
		return jobBuilderFactory.get(JobType.BRIEVEN_GENEREREN.name())
			.listener(listener)
			.start(brievenCleanupStep)
			.next(brievenGenererenPartitionerStep)
			.next(brievenControleStep)
			.build();
	}

	@Bean
	public Step brievenCleanupStep(ColonBriefCleanupReader reader, ColonBriefCleanupWriter writer)
	{
		return stepBuilderFactory.get("brievenCleanupStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step brievenGenererenPartitionerStep(ColonBrievenGenererenPartitioner partitioner, TaskExecutorPartitionHandler brievenPartitionHandler, Step brievenGenererenStep)
	{
		return stepBuilderFactory.get("brievenGenererenPartitionerStep")
			.transactionManager(transactionManager)
			.partitioner("brievenGenererenStep", partitioner)
			.partitionHandler(brievenPartitionHandler)
			.step(brievenGenererenStep)
			.build();
	}

	@Bean
	public TaskExecutorPartitionHandler brievenPartitionHandler(TaskExecutor taskExecutor, Step brievenGenererenStep)
	{
		var partitionHandler = new TaskExecutorPartitionHandler();
		partitionHandler.setTaskExecutor(taskExecutor);
		partitionHandler.setGridSize(10);
		partitionHandler.setStep(brievenGenererenStep);
		return partitionHandler;
	}

	@Bean
	public Step brievenControleStep(ColonBrievenControleReader reader, ColonBrievenControleWriter writer)
	{
		return stepBuilderFactory.get("brievenControleStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step brievenGenererenStep(ColonBrievenGenererenReader reader, ColonBrievenGenererenProcessor processor, ColonBrievenGenererenWriter writer)
	{
		return stepBuilderFactory.get("brievenGenererenStep")
			.transactionManager(transactionManager)
			.<Long, ColonBrief> chunk(50)
			.reader(reader)
			.processor(processor)
			.writer(writer)
			.build();
	}

}
