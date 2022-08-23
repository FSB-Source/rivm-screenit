package nl.rivm.screenit.batch.jobs.colon.ifobtverwerking;

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
import nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.verwerkingstep.IFOBTVerwerkingProcessor;
import nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.verwerkingstep.IFOBTVerwerkingReader;
import nl.rivm.screenit.batch.jobs.colon.ifobtverwerking.verwerkingstep.IFOBTVerwerkingWriter;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class IfobtVerwerkingJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job ifobtVerwerkingJob(IfobtVerwerkingListener listener, Step ifobtVerwerkingStep)
	{
		return jobBuilderFactory.get(JobType.IFOBT_VERWERKING.name())
			.listener(listener)
			.start(ifobtVerwerkingStep)
			.build();
	}

	@Bean
	public Step ifobtVerwerkingStep(IFOBTVerwerkingReader reader, IFOBTVerwerkingProcessor processor, IFOBTVerwerkingWriter writer)
	{
		return stepBuilderFactory.get("ifobtVerwerkingStep")
			.transactionManager(transactionManager)
			.<Long, IFOBTUitslag> chunk(10)
			.reader(reader)
			.processor(processor)
			.writer(writer)
			.build();
	}

}
