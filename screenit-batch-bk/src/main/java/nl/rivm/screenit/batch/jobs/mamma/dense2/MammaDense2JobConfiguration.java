package nl.rivm.screenit.batch.jobs.mamma.dense2;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import nl.rivm.screenit.batch.jobs.mamma.dense2.exporteerstestudieronde.MammaDense2ExportEersteStudierondeTasklet;
import nl.rivm.screenit.batch.jobs.mamma.dense2.exporttweedestudieronde.MammaDense2ExportTweedeStudierondeTasklet;
import nl.rivm.screenit.batch.jobs.mamma.dense2.verwijderdensiteit.MammaVerwijderDensiteitReader;
import nl.rivm.screenit.batch.jobs.mamma.dense2.verwijderdensiteit.MammaVerwijderDensiteitWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MammaDense2JobConfiguration extends AbstractJobConfiguration
{
	@Bean
	public Job dense2CsvExportJob(MammaDense2Listener listener, Step dense2EersteStudierondeExporterenStep, Step dense2TweedeStudierondeExporterenStep,
		Step densiteitVerwijderenStep)
	{
		return jobBuilderFactory.get(JobType.MAMMA_DENSE2_CSV_EXPORT.name())
			.listener(listener)
			.start(dense2EersteStudierondeExporterenStep)
			.next(dense2TweedeStudierondeExporterenStep)
			.next(densiteitVerwijderenStep)
			.build();
	}

	@Bean
	public Step dense2EersteStudierondeExporterenStep(MammaDense2ExportEersteStudierondeTasklet tasklet)
	{
		return stepBuilderFactory.get("dense2EersteStudierondeExporterenStep")
			.transactionManager(transactionManager)
			.tasklet(tasklet)
			.build();
	}

	@Bean
	public Step dense2TweedeStudierondeExporterenStep(MammaDense2ExportTweedeStudierondeTasklet tasklet)
	{
		return stepBuilderFactory.get("dense2TweedeStudierondeExporterenStep")
			.transactionManager(transactionManager)
			.tasklet(tasklet)
			.build();
	}

	@Bean
	public Step densiteitVerwijderenStep(MammaVerwijderDensiteitReader reader, MammaVerwijderDensiteitWriter writer)
	{
		return stepBuilderFactory.get("densiteitVerwijderenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}
}
