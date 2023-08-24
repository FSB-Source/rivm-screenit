package nl.rivm.screenit.batch.jobs.cervix.verrichtingen;

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
import nl.rivm.screenit.batch.jobs.cervix.verrichtingen.cleanup.BetalingBestandenCleanUpReader;
import nl.rivm.screenit.batch.jobs.cervix.verrichtingen.cleanup.BetalingBestandenCleanUpWriter;
import nl.rivm.screenit.batch.jobs.cervix.verrichtingen.dubbelecytologiestep.CervixDubbeleCytologieVerrichtingenReader;
import nl.rivm.screenit.batch.jobs.cervix.verrichtingen.dubbelecytologiestep.CervixDubbeleCytologieVerrichtingenWriter;
import nl.rivm.screenit.batch.jobs.cervix.verrichtingen.mainstep.CervixBepalenVerrichtingenReader;
import nl.rivm.screenit.batch.jobs.cervix.verrichtingen.mainstep.CervixBepalenVerrichtingenWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class CervixBepalenVerrichtingenJobConfiguration extends AbstractJobConfiguration
{

	public static final int CERVIX_BEPALEN_VERRICHTINGEN_JOB_FETCH_SIZE = 50;

	@Bean
	public Job bepalenVerrichtingenJob(CervixBepalenVerrichtingenListener listener, Step bepalenVerrichtingenStep, Step dubbeleCytologieVerrichtingenStep, Step betalingCleanUpStep)
	{
		return jobBuilderFactory.get(JobType.CERVIX_BEPALEN_VERRICHTINGEN.name())
			.listener(listener)
			.start(bepalenVerrichtingenStep)
			.next(dubbeleCytologieVerrichtingenStep)
			.next(betalingCleanUpStep)
			.build();
	}

	@Bean
	public Step bepalenVerrichtingenStep(CervixBepalenVerrichtingenReader reader, CervixBepalenVerrichtingenWriter writer)
	{
		return stepBuilderFactory.get("bepalenVerrichtingenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(100)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step dubbeleCytologieVerrichtingenStep(CervixDubbeleCytologieVerrichtingenReader reader, CervixDubbeleCytologieVerrichtingenWriter writer)
	{
		return stepBuilderFactory.get("dubbeleCytologieVerrichtingenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(100)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step betalingCleanUpStep(BetalingBestandenCleanUpReader reader, BetalingBestandenCleanUpWriter writer)
	{
		return stepBuilderFactory.get("betalingCleanUpStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(1)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
