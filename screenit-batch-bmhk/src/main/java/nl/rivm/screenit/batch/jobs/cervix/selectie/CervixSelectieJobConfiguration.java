package nl.rivm.screenit.batch.jobs.cervix.selectie;

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
import nl.rivm.screenit.batch.jobs.cervix.CervixLabPartitioner;
import nl.rivm.screenit.batch.jobs.cervix.selectie.laatsterondesluitenstep.CervixLaatsteRondeSluitenReader;
import nl.rivm.screenit.batch.jobs.cervix.selectie.laatsterondesluitenstep.CervixLaatsteRondeSluitenWriter;
import nl.rivm.screenit.batch.jobs.cervix.selectie.selectiestep.CervixSelectieReader;
import nl.rivm.screenit.batch.jobs.cervix.selectie.selectiestep.CervixSelectieWriter;
import nl.rivm.screenit.batch.jobs.cervix.selectie.vooraankondiging.CervixVooraankondigingSelectieReader;
import nl.rivm.screenit.batch.jobs.cervix.selectie.vooraankondiging.CervixVooraankondigingSelectieWriter;
import nl.rivm.screenit.batch.jobs.preselectie.ClientPreSelectieTasklet;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.partition.support.TaskExecutorPartitionHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;

@Configuration
public class CervixSelectieJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job selectieJob(CervixSelectieJobListener listener, Step preSelectieStep, Step vooraankondigingPartitionerStep, Step clientSelectiePartitionerStep,
		Step laatsteRondeSluitenStep)
	{
		return jobBuilderFactory.get(JobType.CERVIX_SELECTIE.name())
			.listener(listener)
			.start(preSelectieStep)
			.next(vooraankondigingPartitionerStep)
			.next(clientSelectiePartitionerStep)
			.next(laatsteRondeSluitenStep)
			.build();
	}

	@Bean
	public Step preSelectieStep(ClientPreSelectieTasklet preSelectieTasklet)
	{
		return stepBuilderFactory.get("preSelectieStep")
			.transactionManager(transactionManager)
			.tasklet(preSelectieTasklet)
			.build();
	}

	@Bean
	public ClientPreSelectieTasklet preSelectieTasklet()
	{
		var tasklet = new ClientPreSelectieTasklet();
		tasklet.setBvo(Bevolkingsonderzoek.CERVIX);
		return tasklet;
	}

	@Bean
	public Step vooraankondigingPartitionerStep(CervixLabPartitioner partitioner, TaskExecutorPartitionHandler vooraankondigingPartitionHandler, Step vooraankondigingStep)
	{
		return stepBuilderFactory.get("vooraankondigingPartitionerStep")
			.transactionManager(transactionManager)
			.partitioner("vooraankondigingStep", partitioner)
			.partitionHandler(vooraankondigingPartitionHandler)
			.step(vooraankondigingStep)
			.build();
	}

	@Bean
	public TaskExecutorPartitionHandler vooraankondigingPartitionHandler(TaskExecutor taskExecutor, Step vooraankondigingStep)
	{
		var partitionHandler = new TaskExecutorPartitionHandler();
		partitionHandler.setTaskExecutor(taskExecutor);
		partitionHandler.setGridSize(10);
		partitionHandler.setStep(vooraankondigingStep);
		return partitionHandler;
	}

	@Bean
	public Step vooraankondigingStep(CervixVooraankondigingSelectieReader reader, CervixVooraankondigingSelectieWriter writer)
	{
		return stepBuilderFactory.get("vooraankondigingStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step clientSelectiePartitionerStep(CervixLabPartitioner partitioner, TaskExecutorPartitionHandler selectiePartitionHandler, Step clientSelectieStep)
	{
		return stepBuilderFactory.get("clientSelectiePartitionerStep")
			.transactionManager(transactionManager)
			.partitioner("clientSelectieStep", partitioner)
			.partitionHandler(selectiePartitionHandler)
			.step(clientSelectieStep)
			.build();
	}

	@Bean
	public TaskExecutorPartitionHandler selectiePartitionHandler(TaskExecutor taskExecutor, Step clientSelectieStep)
	{
		var partitionHandler = new TaskExecutorPartitionHandler();
		partitionHandler.setTaskExecutor(taskExecutor);
		partitionHandler.setGridSize(10);
		partitionHandler.setStep(clientSelectieStep);
		return partitionHandler;
	}

	@Bean
	public Step clientSelectieStep(CervixSelectieReader reader, CervixSelectieWriter writer)
	{
		return stepBuilderFactory.get("clientSelectieStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step laatsteRondeSluitenStep(CervixLaatsteRondeSluitenReader reader, CervixLaatsteRondeSluitenWriter writer)
	{
		return stepBuilderFactory.get("laatsteRondeSluitenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
