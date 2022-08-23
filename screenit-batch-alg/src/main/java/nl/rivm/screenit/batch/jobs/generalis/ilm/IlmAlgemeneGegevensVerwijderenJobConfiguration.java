package nl.rivm.screenit.batch.jobs.generalis.ilm;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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
import nl.rivm.screenit.batch.jobs.generalis.ilm.technischelogging.IlmTechnischeBerichtenLoggingVerwijderenReader;
import nl.rivm.screenit.batch.jobs.generalis.ilm.technischelogging.IlmTechnischeBerichtenLoggingVerwijderenWriter;
import nl.rivm.screenit.batch.jobs.ilm.applicatielogging.IlmApplicatieLoggingVerwijderenReader;
import nl.rivm.screenit.batch.jobs.ilm.applicatielogging.IlmApplicatieLoggingVerwijderenWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class IlmAlgemeneGegevensVerwijderenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job ilmAlgemeneGegevensVerwijderenJob(IlmAlgemeneGegevensVerwijderenListener listener, Step ilmApplicatieLoggingVerwijderenStep,
		Step ilmTechnischeBerichtenLoggingVerwijderenStep)
	{
		return jobBuilderFactory.get(JobType.ILM_ALGEMENE_GEGEVENS_VERWIJDEREN.name())
			.listener(listener)
			.start(ilmApplicatieLoggingVerwijderenStep)
			.on("*").to(ilmTechnischeBerichtenLoggingVerwijderenStep)
			.from(ilmApplicatieLoggingVerwijderenStep).on(ExitStatus.FAILED.getExitCode()).to(ilmTechnischeBerichtenLoggingVerwijderenStep)
			.end().build();
	}

	@Bean
	public Step ilmApplicatieLoggingVerwijderenStep(IlmApplicatieLoggingVerwijderenReader reader, IlmApplicatieLoggingVerwijderenWriter writer)
	{
		return stepBuilderFactory.get("ilmApplicatieLoggingVerwijderenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step ilmTechnischeBerichtenLoggingVerwijderenStep(IlmTechnischeBerichtenLoggingVerwijderenReader reader, IlmTechnischeBerichtenLoggingVerwijderenWriter writer)
	{
		return stepBuilderFactory.get("ilmTechnischeBerichtenLoggingVerwijderenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(250)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public IlmApplicatieLoggingVerwijderenReader ilmApplicatieLoggingVerwijderenReader()
	{
		return new IlmApplicatieLoggingVerwijderenReader();
	}

}
