package nl.rivm.screenit.batch.jobs.colon.huisartsberichten;

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
import nl.rivm.screenit.batch.jobs.colon.huisartsberichten.step.ColonMislukteHuisartsberichtenVersturenReader;
import nl.rivm.screenit.batch.jobs.colon.huisartsberichten.step.ColonMislukteHuisartsberichtenVersturenWriter;
import nl.rivm.screenit.model.enums.JobType;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ColonMislukteHuisartsberichtenVersturenJobConfiguration extends AbstractJobConfiguration
{

	@Bean
	public Job huisartsenberichtenOpnieuwVersturenJob(ColonMislukteHuisartsberichtenVersturenListener listener, Step huisartsenberichtenOpnieuwVersturenStep)
	{
		return jobBuilderFactory.get(JobType.COLON_HUISARTSBERICHTEN_OPNIEUW_VERSTUREN.name())
			.listener(listener)
			.start(huisartsenberichtenOpnieuwVersturenStep)
			.build();
	}

	@Bean
	public Step huisartsenberichtenOpnieuwVersturenStep(ColonMislukteHuisartsberichtenVersturenReader reader, ColonMislukteHuisartsberichtenVersturenWriter writer)
	{
		return stepBuilderFactory.get("huisartsenberichtenOpnieuwVersturenStep")
			.transactionManager(transactionManager)
			.<Long, Long> chunk(50)
			.reader(reader)
			.writer(writer)
			.build();
	}

}
