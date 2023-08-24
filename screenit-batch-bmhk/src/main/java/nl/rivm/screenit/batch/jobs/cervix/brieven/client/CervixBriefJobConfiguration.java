package nl.rivm.screenit.batch.jobs.cervix.brieven.client;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.cervix.brieven.client.cleanupstep.CervixBriefCleanupReader;
import nl.rivm.screenit.batch.jobs.cervix.brieven.client.cleanupstep.CervixBriefCleanupWriter;
import nl.rivm.screenit.batch.jobs.cervix.brieven.client.controle.CervixBrievenControleReader;
import nl.rivm.screenit.batch.jobs.cervix.brieven.client.controle.CervixBrievenControleWriter;
import nl.rivm.screenit.batch.jobs.cervix.brieven.client.genererenstep.CervixBrievenGenererenPartitioner;
import nl.rivm.screenit.batch.jobs.cervix.brieven.client.genererenstep.CervixBrievenGenererenProcessor;
import nl.rivm.screenit.batch.jobs.cervix.brieven.client.genererenstep.CervixBrievenGenererenReader;
import nl.rivm.screenit.batch.jobs.cervix.brieven.client.genererenstep.CervixBrievenGenererenWriter;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.partition.support.TaskExecutorPartitionHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

@Configuration
public class CervixBriefJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job brievenJob(CervixBriefListener listener, Step brievenCleanupStep, Step brievenGenererenPartitionerStep, Step brievenControleStep)
	{
		return jobBuilderFactory.get(JobType.CERVIX_BRIEVEN.name())
			.listener(listener)
			.start(brievenCleanupStep)
			.next(brievenGenererenPartitionerStep)
			.next(brievenControleStep)
			.build();
	}

	@Bean
	public Step brievenCleanupStep(CervixBriefCleanupReader reader, CervixBriefCleanupWriter writer)
	{
		return stepBuilderFactory.get("brievenCleanupStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step brievenGenererenPartitionerStep(CervixBrievenGenererenPartitioner partitioner, TaskExecutorPartitionHandler brievenPartitionHandler, Step brievenGenererenStep)
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
	public Step brievenControleStep(CervixBrievenControleReader reader, CervixBrievenControleWriter writer)
	{
		return stepBuilderFactory.get("brievenControleStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step brievenGenererenStep(CervixBrievenGenererenReader reader, CervixBrievenGenererenProcessor processor, CervixBrievenGenererenWriter writer)
	{
		return stepBuilderFactory.get("brievenGenererenStep")
			.transactionManager(transactionManager)
			.<Long, CervixBrief> chunk(50)
			.reader(reader)
			.processor(processor)
			.writer(writer)
			.build();
	}

}
