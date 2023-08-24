package nl.rivm.screenit.batch.jobs.cervix.hpvoru;

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
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.partition.support.TaskExecutorPartitionHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

@Configuration
public class CervixHpvOruBerichtenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job oruBerichtenJob(CervixHpvOruBerichtenListener listener, Step oruBerichtenPartitionerStep)
	{
		return jobBuilderFactory.get(JobType.CERVIX_HPV_ORU.name())
			.listener(listener)
			.start(oruBerichtenPartitionerStep)
			.build();
	}

	@Bean
	public Step oruBerichtenPartitionerStep(CervixHpvOruBerichtenPartitioner partitioner, TaskExecutorPartitionHandler oruBerichtenPartitionHandler, Step oruVersturenStep)
	{
		return stepBuilderFactory.get("oruBerichtenPartitionerStep")
			.transactionManager(transactionManager)
			.partitioner("oruVersturenStep", partitioner)
			.partitionHandler(oruBerichtenPartitionHandler)
			.step(oruVersturenStep)
			.build();
	}

	@Bean
	public TaskExecutorPartitionHandler oruBerichtenPartitionHandler(TaskExecutor threadTaskExecutor, Step oruVersturenStep)
	{
		var partitionHandler = new TaskExecutorPartitionHandler();
		partitionHandler.setTaskExecutor(threadTaskExecutor);
		partitionHandler.setStep(oruVersturenStep);
		partitionHandler.setGridSize(5);
		return partitionHandler;
	}

	@Bean
	public Step oruVersturenStep(CervixHpvOruBerichtenReader reader, CervixHpvOruBerichtenWriter writer)
	{
		return stepBuilderFactory.get("oruVersturenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public CervixHpvOruBerichtenPartitioner oruBerichtenPartitioner()
	{
		var partitioner = new CervixHpvOruBerichtenPartitioner();
		partitioner.setTimeout(60);
		return partitioner;
	}

}
