package nl.rivm.screenit.batch.jobs.cervix.oudenietverstuurdezas;

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
import nl.rivm.screenit.batch.jobs.cervix.oudenietverstuurdezas.step.CervixOudeNietIngestuurdeZasDecider;
import nl.rivm.screenit.batch.jobs.cervix.oudenietverstuurdezas.step.CervixOudeNietIngestuurdeZasReader;
import nl.rivm.screenit.batch.jobs.cervix.oudenietverstuurdezas.step.CervixOudeNietIngestuurdeZasWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class CervixOudeNietIngestuurdeZasJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job oudeNietIngestuurdeZasJob(CervixOudeNietIngestuurdeZasJobListener listener, Step dummyStep, CervixOudeNietIngestuurdeZasDecider oudeNietIngestuurdeZasDecider,
		Step oudeNietIngestuurdeZasStep)
	{
		return jobBuilderFactory.get(JobType.CERVIX_OUDE_NIET_INGESTUURDE_ZAS.name())
			.listener(listener)
			.start(dummyStep)
			.next(oudeNietIngestuurdeZasDecider)
			.on(ExitStatus.FAILED.getExitCode()).to(oudeNietIngestuurdeZasStep)
			.from(oudeNietIngestuurdeZasDecider)
			.on(ExitStatus.COMPLETED.getExitCode()).end()
			.end().build();
	}

	@Bean
	public Step oudeNietIngestuurdeZasStep(CervixOudeNietIngestuurdeZasReader reader, CervixOudeNietIngestuurdeZasWriter writer)
	{
		return stepBuilderFactory.get("oudeNietIngestuurdeZasStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(1)
			.reader(reader)
			.writer(writer)
			.build();
	}
}
