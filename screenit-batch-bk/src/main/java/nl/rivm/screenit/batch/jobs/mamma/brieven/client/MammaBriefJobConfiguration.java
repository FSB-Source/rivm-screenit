package nl.rivm.screenit.batch.jobs.mamma.brieven.client;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import nl.rivm.screenit.batch.jobs.mamma.brieven.client.cleanupstep.MammaBriefCleanupReader;
import nl.rivm.screenit.batch.jobs.mamma.brieven.client.cleanupstep.MammaBriefCleanupWriter;
import nl.rivm.screenit.batch.jobs.mamma.brieven.client.controle.MammaBrievenControleReader;
import nl.rivm.screenit.batch.jobs.mamma.brieven.client.controle.MammaBrievenControleWriter;
import nl.rivm.screenit.batch.jobs.mamma.brieven.client.genererenstep.MammaBrievenGenererenPartitioner;
import nl.rivm.screenit.batch.jobs.mamma.brieven.client.genererenstep.MammaBrievenGenererenProcessor;
import nl.rivm.screenit.batch.jobs.mamma.brieven.client.genererenstep.MammaBrievenGenererenReader;
import nl.rivm.screenit.batch.jobs.mamma.brieven.client.genererenstep.MammaBrievenGenererenWriter;
import nl.rivm.screenit.model.enums.JobType;
import nl.rivm.screenit.model.mamma.MammaBrief;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.partition.support.TaskExecutorPartitionHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

@Configuration
public class MammaBriefJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job brievenJob(MammaBriefListener listener, Step brievenCleanupStep, Step brievenGenererenPartitionerStep, Step brievenControleStep)
	{
		return jobBuilderFactory.get(JobType.MAMMA_BRIEVEN.name())
			.listener(listener)
			.start(brievenCleanupStep)
			.next(brievenGenererenPartitionerStep)
			.next(brievenControleStep)
			.build();
	}

	@Bean
	public Step brievenCleanupStep(MammaBriefCleanupReader reader, MammaBriefCleanupWriter writer)
	{
		return stepBuilderFactory.get("brievenCleanupStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step brievenGenererenPartitionerStep(MammaBrievenGenererenPartitioner partitioner, TaskExecutorPartitionHandler brievenPartitionHandler, Step brievenGenererenStep)
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
	public Step brievenControleStep(MammaBrievenControleReader reader, MammaBrievenControleWriter writer)
	{
		return stepBuilderFactory.get("brievenControleStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step brievenGenererenStep(MammaBrievenGenererenReader reader, MammaBrievenGenererenProcessor processor, MammaBrievenGenererenWriter writer)
	{
		return stepBuilderFactory.get("brievenGenererenStep")
			.transactionManager(transactionManager)
			.<Long, MammaBrief> chunk(50)
			.reader(reader)
			.processor(processor)
			.writer(writer)
			.build();
	}

}
