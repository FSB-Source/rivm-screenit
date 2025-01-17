package nl.rivm.screenit.batch.jobs.cervix.uitslagverwijderen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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
import nl.rivm.screenit.batch.jobs.cervix.uitslagverwijderen.cervixlabformuliersphereonverwijderen.CervixOudeLabformulierenVerwijderenReader;
import nl.rivm.screenit.batch.jobs.cervix.uitslagverwijderen.cervixlabformuliersphereonverwijderen.CervixOudeLabformulierenVerwijderenWriter;
import nl.rivm.screenit.batch.jobs.cervix.uitslagverwijderen.hpvminformulierverwijderen.CervixHpvMinFormulierVerwijderenReader;
import nl.rivm.screenit.batch.jobs.cervix.uitslagverwijderen.hpvminformulierverwijderen.CervixHpvMinFormulierVerwijderenWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class CervixHpvMinFormulierVerwijderenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job hpvminVerwijderenUitslagJob(CervixHpvMinFormulierVerwijderenListener listener, Step hpvMinFormulierVerwijderenStep, Step oudeLabformulierenVerwijderenStep)
	{
		return jobBuilderFactory.get(JobType.CERVIX_HPVMIN_VERWIJDEREN_UITSLAG.name())
			.listener(listener)
			.start(hpvMinFormulierVerwijderenStep)
			.next(oudeLabformulierenVerwijderenStep)
			.build();
	}

	@Bean
	public Step hpvMinFormulierVerwijderenStep(CervixHpvMinFormulierVerwijderenReader reader, CervixHpvMinFormulierVerwijderenWriter writer)
	{
		return stepBuilderFactory.get("hpvMinFormulierVerwijderenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(100)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step oudeLabformulierenVerwijderenStep(CervixOudeLabformulierenVerwijderenReader reader, CervixOudeLabformulierenVerwijderenWriter writer)
	{
		return stepBuilderFactory.get("oudeLabformulierenVerwijderenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}
}
