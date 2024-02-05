package nl.rivm.screenit.batch.jobs.mamma.onderzoek;

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
import nl.rivm.screenit.batch.jobs.mamma.onderzoek.geenbeelden.MammaOnderzoekenZonderBeeldenReader;
import nl.rivm.screenit.batch.jobs.mamma.onderzoek.geenbeelden.MammaOnderzoekenZonderBeeldenWriter;
import nl.rivm.screenit.batch.jobs.mamma.onderzoek.nietafgesloten.MammaOnderzoekenNietAfgeslotenReader;
import nl.rivm.screenit.batch.jobs.mamma.onderzoek.nietafgesloten.MammaOnderzoekenNietAfgeslotenWriter;
import nl.rivm.screenit.batch.jobs.mamma.onderzoek.onderbrokenonderzoeken.MammaVervolgTeOudeOnderbrokenOnderzoekenReader;
import nl.rivm.screenit.batch.jobs.mamma.onderzoek.onderbrokenonderzoeken.MammaVervolgTeOudeOnderbrokenOnderzoekenWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MammaVervolgOnderzoekenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job vervolgOnderzoekenJob(MammaVervolgOnderzoekenListener listener, Step onderbrokenOnderzoekenStep, Step geenBeeldenStep, Step nietAfgeslotenStep)
	{
		return jobBuilderFactory.get(JobType.MAMMA_VERVOLG_ONDERZOEKEN.name())
			.listener(listener)
			.start(onderbrokenOnderzoekenStep)
			.on("*").to(geenBeeldenStep)
			.from(onderbrokenOnderzoekenStep).on(ExitStatus.FAILED.getExitCode()).to(geenBeeldenStep)
			.from(geenBeeldenStep)
			.on("*").to(nietAfgeslotenStep)
			.from(geenBeeldenStep).on(ExitStatus.FAILED.getExitCode()).to(nietAfgeslotenStep)
			.from(nietAfgeslotenStep)
			.end().build();
	}

	@Bean
	public Step onderbrokenOnderzoekenStep(MammaVervolgTeOudeOnderbrokenOnderzoekenReader reader, MammaVervolgTeOudeOnderbrokenOnderzoekenWriter writer)
	{
		return stepBuilderFactory.get("onderbrokenOnderzoekenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step geenBeeldenStep(MammaOnderzoekenZonderBeeldenReader reader, MammaOnderzoekenZonderBeeldenWriter writer)
	{
		return stepBuilderFactory.get("geenBeeldenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step nietAfgeslotenStep(MammaOnderzoekenNietAfgeslotenReader reader, MammaOnderzoekenNietAfgeslotenWriter writer)
	{
		return stepBuilderFactory.get("nietAfgeslotenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
