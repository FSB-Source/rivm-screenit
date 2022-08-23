package nl.rivm.screenit.batch.jobs.cervix.aftergba;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.AbstractJobConfiguration;
import nl.rivm.screenit.batch.jobs.cervix.aftergba.deelnamemodus.CervixDeelnamemodusReader;
import nl.rivm.screenit.batch.jobs.cervix.aftergba.deelnamemodus.CervixDeelnamemodusWriter;
import nl.rivm.screenit.batch.jobs.cervix.aftergba.retourzendingstep.RetourzendingReader;
import nl.rivm.screenit.batch.jobs.cervix.aftergba.retourzendingstep.RetourzendingWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class AfterGbaJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job afterGbaJob(AfterGbaListener listener, Step deelnamemodusBijwerkenStep, Step retourzendingStep)
	{
		return jobBuilderFactory.get(JobType.CERVIX_NA_GBA.name())
			.listener(listener)
			.start(deelnamemodusBijwerkenStep)
			.on("*").to(retourzendingStep)
			.from(deelnamemodusBijwerkenStep)
			.on(ExitStatus.FAILED.getExitCode()).to(retourzendingStep)
			.end().build();
	}

	@Bean
	public Step deelnamemodusBijwerkenStep(CervixDeelnamemodusReader reader, CervixDeelnamemodusWriter writer)
	{
		return stepBuilderFactory.get("deelnamemodusBijwerkenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step retourzendingStep(RetourzendingReader reader, RetourzendingWriter writer)
	{
		return stepBuilderFactory.get("retourzendingStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
