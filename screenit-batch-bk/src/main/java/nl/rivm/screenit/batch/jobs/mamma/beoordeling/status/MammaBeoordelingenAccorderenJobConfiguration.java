package nl.rivm.screenit.batch.jobs.mamma.beoordeling.status;

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
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.status.step.MammaBeoordelingenAccorderenReader;
import nl.rivm.screenit.batch.jobs.mamma.beoordeling.status.step.MammaBeoordelingenAccorderenWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MammaBeoordelingenAccorderenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job beoordelingenAccorderenJob(MammaBeoordelingenAccorderenListener listener, Step beoordelingenAccorderenStep)
	{
		return jobBuilderFactory.get(JobType.MAMMA_BEOORDELINGEN_ACCORDEREN.name())
			.listener(listener)
			.start(beoordelingenAccorderenStep)
			.build();
	}

	@Bean
	public Step beoordelingenAccorderenStep(MammaBeoordelingenAccorderenReader reader, MammaBeoordelingenAccorderenWriter writer)
	{
		return stepBuilderFactory.get("beoordelingenAccorderenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}
}
