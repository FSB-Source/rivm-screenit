package nl.rivm.screenit.batch.jobs.colon.intake;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import nl.rivm.screenit.batch.jobs.colon.intake.afsprakenmakenstep.IntakeAfsprakenMakenReader;
import nl.rivm.screenit.batch.jobs.colon.intake.afsprakenmakenstep.IntakeAfsprakenMakenWriter;
import nl.rivm.screenit.batch.model.ClientAfspraak;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class IntakeAfsprakenMakenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job intakeAfsprakenMakenJob(IntakeAfsprakenMakenListener listener, Step dummyStep, IntakeAfsprakenMakenDecider decider, Step intakeAfsprakenMakenStep)
	{
		return jobBuilderFactory.get(JobType.INTAKE_AFSPRAKEN_MAKEN.name())
			.listener(listener)
			.start(dummyStep)
			.next(decider)
			.on(ExitStatus.FAILED.getExitCode()).to(intakeAfsprakenMakenStep)
			.from(decider).on(ExitStatus.COMPLETED.getExitCode()).end()
			.from(intakeAfsprakenMakenStep).on("*").to(decider).end()
			.build();
	}

	@Bean
	public Step intakeAfsprakenMakenStep(IntakeAfsprakenMakenReader reader, IntakeAfsprakenMakenWriter writer)
	{
		return stepBuilderFactory.get("intakeAfsprakenMakenStep")
			.transactionManager(transactionManager)
			.<ClientAfspraak, ClientAfspraak> chunk(20)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
