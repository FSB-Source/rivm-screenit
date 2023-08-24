package nl.rivm.screenit.batch.jobs.cervix.order;

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
import nl.rivm.screenit.batch.jobs.cervix.order.aanmaakstep.CervixOrderAanmaakReader;
import nl.rivm.screenit.batch.jobs.cervix.order.aanmaakstep.CervixOrderAanmaakWriter;
import nl.rivm.screenit.batch.jobs.cervix.order.versturenstep.CervixOrderVersturenReader;
import nl.rivm.screenit.batch.jobs.cervix.order.versturenstep.CervixOrderVersturenWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class CervixOrderJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job orderJob(CervixOrderListener listener, Step orderAanmaakStep, Step orderVersturenStep)
	{
		return jobBuilderFactory.get(JobType.CERVIX_ORDER.name())
			.listener(listener)
			.start(orderAanmaakStep)
			.next(orderVersturenStep)
			.build();
	}

	@Bean
	public Step orderAanmaakStep(CervixOrderAanmaakReader reader, CervixOrderAanmaakWriter writer)
	{
		return stepBuilderFactory.get("orderAanmaakStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step orderVersturenStep(CervixOrderVersturenReader reader, CervixOrderVersturenWriter writer)
	{
		return stepBuilderFactory.get("orderVersturenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(1) 
			.reader(reader)
			.writer(writer)
			.build();
	}

}
