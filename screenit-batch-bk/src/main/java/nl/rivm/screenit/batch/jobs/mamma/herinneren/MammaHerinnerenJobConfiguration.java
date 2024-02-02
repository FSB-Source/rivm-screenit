package nl.rivm.screenit.batch.jobs.mamma.herinneren;

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
import nl.rivm.screenit.batch.jobs.mamma.herinneren.step.MammaHerinnerenReader;
import nl.rivm.screenit.batch.jobs.mamma.herinneren.step.MammaHerinnerenWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MammaHerinnerenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job herinnerenJob(MammaHerinnerenListener listener, Step herinnerenStep)
	{
		return jobBuilderFactory.get(JobType.MAMMA_HERINNEREN.name())
			.listener(listener)
			.start(herinnerenStep)
			.build();
	}

	@Bean
	public Step herinnerenStep(MammaHerinnerenReader reader, MammaHerinnerenWriter writer)
	{
		return stepBuilderFactory.get("herinnerenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}
}
