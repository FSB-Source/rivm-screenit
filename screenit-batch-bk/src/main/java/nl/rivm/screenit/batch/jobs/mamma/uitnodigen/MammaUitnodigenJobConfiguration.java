package nl.rivm.screenit.batch.jobs.mamma.uitnodigen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import nl.rivm.screenit.batch.jobs.mamma.uitnodigen.afronden.MammaVerlopenRondesReader;
import nl.rivm.screenit.batch.jobs.mamma.uitnodigen.afronden.MammaVerlopenRondesWriter;
import nl.rivm.screenit.batch.jobs.mamma.uitnodigen.interval.MammaIntervalUitnodigenReader;
import nl.rivm.screenit.batch.jobs.mamma.uitnodigen.interval.MammaIntervalUitnodigenWriter;
import nl.rivm.screenit.batch.jobs.mamma.uitnodigen.rondefactor.MammaEersteOnderzoekBijwerkenReader;
import nl.rivm.screenit.batch.jobs.mamma.uitnodigen.rondefactor.MammaEersteOnderzoekBijwerkenWriter;
import nl.rivm.screenit.batch.jobs.mamma.uitnodigen.step.uitnodigen.MammaUitnodigenTasklet;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MammaUitnodigenJobConfiguration extends AbstractJobConfiguration
{
	@Bean
	public Job uitnodigenJob(MammaUitnodigenListener listener, Step eersteOnderzoekBijwerkenStep, Step intervalUitnodigenStep, Step uitnodigenStep, Step afrondenStep)
	{
		return jobBuilderFactory.get(JobType.MAMMA_UITNODIGEN.name())
			.listener(listener)
			.start(eersteOnderzoekBijwerkenStep)
			.on("*").to(intervalUitnodigenStep)
			.from(eersteOnderzoekBijwerkenStep).on(ExitStatus.FAILED.getExitCode()).to(intervalUitnodigenStep)
			.from(intervalUitnodigenStep).on("*").to(uitnodigenStep)
			.from(intervalUitnodigenStep).on(ExitStatus.FAILED.getExitCode()).to(uitnodigenStep)
			.from(uitnodigenStep).on("*").to(afrondenStep)
			.from(uitnodigenStep).on(ExitStatus.FAILED.getExitCode()).to(afrondenStep)
			.from(afrondenStep)
			.end().build();
	}

	@Bean
	public Step eersteOnderzoekBijwerkenStep(MammaEersteOnderzoekBijwerkenReader reader, MammaEersteOnderzoekBijwerkenWriter writer)
	{
		return stepBuilderFactory.get("eersteOnderzoekBijwerkenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step intervalUitnodigenStep(MammaIntervalUitnodigenReader reader, MammaIntervalUitnodigenWriter writer)
	{
		return stepBuilderFactory.get("intervalUitnodigenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step uitnodigenStep(MammaUitnodigenTasklet tasklet)
	{
		return stepBuilderFactory.get("uitnodigenStep")
			.transactionManager(transactionManager)
			.tasklet(tasklet)
			.build();
	}

	@Bean
	public Step afrondenStep(MammaVerlopenRondesReader reader, MammaVerlopenRondesWriter writer)
	{
		return stepBuilderFactory.get("afrondenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
