package nl.rivm.screenit.batch.jobs.colon.oneindigetijdsloten;

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
import nl.rivm.screenit.batch.jobs.colon.KoppelPromotionListener;
import nl.rivm.screenit.batch.jobs.colon.oneindigetijdsloten.blokkades.OneindigeBlokkadeReader;
import nl.rivm.screenit.batch.jobs.colon.oneindigetijdsloten.blokkades.OneindigeBlokkadeWriter;
import nl.rivm.screenit.batch.jobs.colon.oneindigetijdsloten.roosteritems.OneindigeRoosterItemReader;
import nl.rivm.screenit.batch.jobs.colon.oneindigetijdsloten.roosteritems.OneindigeRoosterItemWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class OneindigeTijdslotenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job oneindigeRoosteritemsUitrollenJob(OneindigeTijdslotenListener listener, KoppelPromotionListener koppelPromotionListener, Step roosterItemsUitrollenStep,
		Step blokkadesUitrollenStep)
	{
		return jobBuilderFactory.get(JobType.ONEINDIGE_ROOSTERITEMS_UITROLLEN.name())
			.listener(listener)
			.listener(koppelPromotionListener)
			.start(roosterItemsUitrollenStep)
			.next(blokkadesUitrollenStep)
			.build();
	}

	@Bean
	public Step roosterItemsUitrollenStep(OneindigeRoosterItemReader reader, OneindigeRoosterItemWriter writer)
	{
		return stepBuilderFactory.get("roosterItemsUitrollenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

	@Bean
	public Step blokkadesUitrollenStep(OneindigeBlokkadeReader reader, OneindigeBlokkadeWriter writer)
	{
		return stepBuilderFactory.get("blokkadesUitrollenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(10)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
