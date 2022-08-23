package nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.cleanupstep.BezwaarBrievenCleanUpReader;
import nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.cleanupstep.BezwaarBrievenCleanUpWriter;
import nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.controlestep.BezwaarBrievenControleReader;
import nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.controlestep.BezwaarBrievenControleWriter;
import nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.genererenstep.BezwaarBrievenGenererenPartitioner;
import nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.genererenstep.BezwaarBrievenGenererenProcessor;
import nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.genererenstep.BezwaarBrievenGenererenReader;
import nl.rivm.screenit.batch.jobs.generalis.brieven.bezwaar.genererenstep.BezwaarBrievenGenererenWriter;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.partition.support.TaskExecutorPartitionHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

@Configuration
@AllArgsConstructor
public class BezwaarBrievenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job bezwaarBrievenJob(BezwaarBrievenListener listener, Step bezwaarBrievenCleanupStep, Step bezwaarBrievenPartitionerStep, Step bezwaarBrievenControleStep)
	{
		return jobBuilderFactory.get(JobType.BEZWAAR_BRIEVEN.name())
			.listener(listener)
			.start(bezwaarBrievenCleanupStep)
			.next(bezwaarBrievenPartitionerStep)
			.next(bezwaarBrievenControleStep)
			.build();
	}

	@Bean
	public Step bezwaarBrievenCleanupStep(BezwaarBrievenCleanUpReader reader, BezwaarBrievenCleanUpWriter writer)
	{
		return stepBuilderFactory.get("bezwaarBrievenCleanupStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step bezwaarBrievenPartitionerStep(BezwaarBrievenGenererenPartitioner partitioner, Step bezwaarBrievenGenererenStep,
		TaskExecutorPartitionHandler bezwaarBrievenPartitionHandler)
	{
		return stepBuilderFactory.get("bezwaarBrievenPartitionerStep")
			.transactionManager(transactionManager)
			.partitioner("bezwaarBrievenGenererenStep", partitioner)
			.partitionHandler(bezwaarBrievenPartitionHandler)
			.step(bezwaarBrievenGenererenStep)
			.build();
	}

	@Bean
	public Step bezwaarBrievenControleStep(BezwaarBrievenControleReader reader, BezwaarBrievenControleWriter writer)
	{
		return stepBuilderFactory.get("bezwaarBrievenControleStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step bezwaarBrievenGenererenStep(BezwaarBrievenGenererenReader reader, BezwaarBrievenGenererenProcessor processor, BezwaarBrievenGenererenWriter writer)
	{
		return stepBuilderFactory.get("bezwaarBrievenGenererenStep")
			.transactionManager(transactionManager)
			.<Long, BezwaarBrief> chunk(250)
			.reader(reader)
			.processor(processor)
			.writer(writer)
			.build();
	}

	@Bean
	public TaskExecutorPartitionHandler bezwaarBrievenPartitionHandler(TaskExecutor taskExecutor, Step bezwaarBrievenGenererenStep)
	{
		var partitionHandler = new TaskExecutorPartitionHandler();
		partitionHandler.setTaskExecutor(taskExecutor);
		partitionHandler.setStep(bezwaarBrievenGenererenStep);
		return partitionHandler;
	}

}
