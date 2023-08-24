package nl.rivm.screenit.batch.jobs.generalis.huisarts;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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
import nl.rivm.screenit.batch.jobs.generalis.huisarts.afterimportstep.AfterImportEnovationHuisartsTasklet;
import nl.rivm.screenit.batch.jobs.generalis.huisarts.importstep.EnovationHuisartsCsvFileProvider;
import nl.rivm.screenit.batch.jobs.generalis.huisarts.importstep.EnovationHuisartsReader;
import nl.rivm.screenit.batch.jobs.generalis.huisarts.importstep.EnovationHuisartsWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class EnovationHuisartsJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job enovationHuisartsenBatchJob(EnovationHuisartsJobListener listener, Step huisartsInfoUpdateStep, Step runAfterJobsStep)
	{
		return jobBuilderFactory.get(JobType.ENOVATION_HUISARTSEN_BATCH.name())
			.listener(listener)
			.start(huisartsInfoUpdateStep)
			.next(runAfterJobsStep)
			.build();
	}

	@Bean
	public Step huisartsInfoUpdateStep(EnovationHuisartsReader reader, EnovationHuisartsWriter writer)
	{
		return stepBuilderFactory.get("huisartsInfoUpdateStep")
			.transactionManager(transactionManager)
			.<Object[], Object[]> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step runAfterJobsStep(AfterImportEnovationHuisartsTasklet tasklet)
	{
		return stepBuilderFactory.get("runAfterJobsStep")
			.transactionManager(transactionManager)
			.tasklet(tasklet)
			.build();
	}

	@Bean
	public EnovationHuisartsReader enovationHuisartsReader(EnovationHuisartsCsvFileProvider csvFileProvider)
	{
		var reader = new EnovationHuisartsReader();
		reader.setCsvFileProvider(csvFileProvider);
		return reader;
	}
}
