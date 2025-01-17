package nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.briefandereintakelocatie.HerinneringClientWilAnderIntakeLocatieBriefReader;
import nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.briefandereintakelocatie.HerinneringClientWilAnderIntakeLocatieBriefWriter;
import nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.onafgerondeverwijzing.OnbevestigdeIntakeVerwijzingenReader;
import nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.onafgerondeverwijzing.OnbevestigdeIntakeVerwijzingenWriter;
import nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.versturennoshow.HuisartsNoShowReader;
import nl.rivm.screenit.batch.jobs.colon.vervolgintakeconclusie.versturennoshow.HuisartsNoShowWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class VervolgIntakeConclusieJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job vervolgIntakeConclusieJob(VervolgIntakeConclusieJobListener listener, Step verstuurNoShowBerichten, Step herinneringClientWilAndereIntakeLocatieStep,
		Step onbevestigdeIntakeVerwijzingenStep)
	{
		return jobBuilderFactory.get(JobType.VERVOLG_INTAKE_CONCLUSIE_BATCH.name())
			.listener(listener)
			.start(verstuurNoShowBerichten)
			.on("*").to(herinneringClientWilAndereIntakeLocatieStep)
			.from(verstuurNoShowBerichten).on(ExitStatus.FAILED.getExitCode()).to(herinneringClientWilAndereIntakeLocatieStep)
			.from(herinneringClientWilAndereIntakeLocatieStep).next(onbevestigdeIntakeVerwijzingenStep)
			.end()
			.build();
	}

	@Bean
	public Step verstuurNoShowBerichten(HuisartsNoShowReader reader, HuisartsNoShowWriter writer)
	{
		return stepBuilderFactory.get("verstuurNoShowBerichten")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step herinneringClientWilAndereIntakeLocatieStep(HerinneringClientWilAnderIntakeLocatieBriefReader reader, HerinneringClientWilAnderIntakeLocatieBriefWriter writer)
	{
		return stepBuilderFactory.get("herinneringClientWilAndereIntakeLocatieStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step onbevestigdeIntakeVerwijzingenStep(OnbevestigdeIntakeVerwijzingenReader reader, OnbevestigdeIntakeVerwijzingenWriter writer)
	{
		return stepBuilderFactory.get("onbevestigdeIntakeVerwijzingenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
