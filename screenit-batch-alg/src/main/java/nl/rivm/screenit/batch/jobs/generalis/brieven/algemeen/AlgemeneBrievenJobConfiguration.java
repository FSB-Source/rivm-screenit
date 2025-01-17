package nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen.cleanupstep.AlgemeneBrievenCleanupReader;
import nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen.cleanupstep.AlgemeneBrievenCleanupWriter;
import nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen.controlestep.AlgemeneBrievenControleReader;
import nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen.controlestep.AlgemeneBrievenControleWriter;
import nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen.genererenstep.AlgemeneBrievenGenererenPartitioner;
import nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen.genererenstep.AlgemeneBrievenGenererenProcessor;
import nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen.genererenstep.AlgemeneBrievenGenererenReader;
import nl.rivm.screenit.batch.jobs.generalis.brieven.algemeen.genererenstep.AlgemeneBrievenGenererenWriter;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.partition.support.TaskExecutorPartitionHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

@Configuration
@AllArgsConstructor
public class AlgemeneBrievenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job algemeneBrievenJob(AlgemeneBrievenListener listener, Step algemeneBrievenCleanupStep, Step algemeneBrievenPartitionerStep, Step algemeneBrievenControleStep)
	{
		return jobBuilderFactory.get(JobType.ALGEMENE_BRIEVEN.name())
			.listener(listener)
			.start(algemeneBrievenCleanupStep)
			.next(algemeneBrievenPartitionerStep)
			.next(algemeneBrievenControleStep)
			.build();
	}

	@Bean
	public Step algemeneBrievenCleanupStep(AlgemeneBrievenCleanupReader reader, AlgemeneBrievenCleanupWriter writer)
	{
		return stepBuilderFactory.get("algemeneBrievenCleanupStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step algemeneBrievenPartitionerStep(AlgemeneBrievenGenererenPartitioner partitioner, Step algemeneBrievenGenererenStep,
		TaskExecutorPartitionHandler algemeneBrievenPartitionHandler)
	{
		return stepBuilderFactory.get("algemeneBrievenPartitionerStep")
			.transactionManager(transactionManager)
			.partitioner("algemeneBrievenGenererenStep", partitioner)
			.partitionHandler(algemeneBrievenPartitionHandler)
			.step(algemeneBrievenGenererenStep)
			.build();
	}

	@Bean
	public Step algemeneBrievenControleStep(AlgemeneBrievenControleReader reader, AlgemeneBrievenControleWriter writer)
	{
		return stepBuilderFactory.get("algemeneBrievenControleStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step algemeneBrievenGenererenStep(AlgemeneBrievenGenererenReader reader, AlgemeneBrievenGenererenProcessor processor, AlgemeneBrievenGenererenWriter writer)
	{
		return stepBuilderFactory.get("algemeneBrievenGenererenStep")
			.transactionManager(transactionManager)
			.<Long, AlgemeneBrief> chunk(250)
			.reader(reader)
			.processor(processor)
			.writer(writer)
			.build();
	}

	@Bean
	public TaskExecutorPartitionHandler algemeneBrievenPartitionHandler(TaskExecutor taskExecutor, Step algemeneBrievenGenererenStep)
	{
		var partitionHandler = new TaskExecutorPartitionHandler();
		partitionHandler.setTaskExecutor(taskExecutor);
		partitionHandler.setStep(algemeneBrievenGenererenStep);
		return partitionHandler;
	}

}
