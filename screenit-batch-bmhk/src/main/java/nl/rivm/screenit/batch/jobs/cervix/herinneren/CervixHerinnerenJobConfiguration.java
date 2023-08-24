package nl.rivm.screenit.batch.jobs.cervix.herinneren;

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
import nl.rivm.screenit.batch.jobs.cervix.herinneren.allsteps.CervixHerinnerenWriter;
import nl.rivm.screenit.batch.jobs.cervix.herinneren.uitstrijkjestep.CervixUitstrijkjeHerinnerenReader;
import nl.rivm.screenit.batch.jobs.cervix.herinneren.zasnonresponderstep.CervixZasNonResponderHerinnerenReader;
import nl.rivm.screenit.batch.jobs.cervix.herinneren.zaspustep.CervixZasPrimaireUitnodigingHerinnerenReader;
import nl.rivm.screenit.batch.jobs.cervix.herinneren.zasstep.CervixAangevraagdeZasHerinnerenReader;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class CervixHerinnerenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job herinnerenJob(CervixHerinnerenJobListener listener, Step herinnerenZasStep, Step herinnerenUitstrijkjeStep, Step herinnerenZasNonresponderStep,
		Step herinnerenZasPuStep)
	{
		return jobBuilderFactory.get(JobType.CERVIX_HERINNEREN.name())
			.listener(listener)
			.start(herinnerenZasStep)
			.next(herinnerenUitstrijkjeStep)
			.next(herinnerenZasPuStep)
			.next(herinnerenZasNonresponderStep)
			.build();
	}

	@Bean
	public Step herinnerenZasStep(CervixAangevraagdeZasHerinnerenReader reader, CervixHerinnerenWriter writer)
	{
		return stepBuilderFactory.get("herinnerenZasStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step herinnerenUitstrijkjeStep(CervixUitstrijkjeHerinnerenReader reader, CervixHerinnerenWriter writer)
	{
		return stepBuilderFactory.get("herinnerenUitstrijkjeStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step herinnerenZasNonresponderStep(CervixZasNonResponderHerinnerenReader reader, CervixHerinnerenWriter writer)
	{
		return stepBuilderFactory.get("herinnerenZasNonresponderStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step herinnerenZasPuStep(CervixZasPrimaireUitnodigingHerinnerenReader reader, CervixHerinnerenWriter writer)
	{
		return stepBuilderFactory.get("herinnerenZasPuStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
