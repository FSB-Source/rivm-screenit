package nl.rivm.screenit.batch.jobs.cervix.huisartsberichten.huisartsbepalenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.cervix.CervixHuisartsBericht;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsBerichtStatus;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.cervix.huisartsberichten.CervixHuisartsberichtenJobConfiguration.CERVIX_HUISARTSENBERICHTEN_JOB_READERS_FETCH_SIZE;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsBerichtSpecification.heeftGeenHuisartsLocatie;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsBerichtSpecification.heeftStatus;

@Component
public class CervixHuisartsBepalenReader extends BaseSpecificationScrollableResultReader<CervixHuisartsBericht>
{

	public CervixHuisartsBepalenReader()
	{
		super.setFetchSize(CERVIX_HUISARTSENBERICHTEN_JOB_READERS_FETCH_SIZE);
	}

	@Override
	protected Specification<CervixHuisartsBericht> createSpecification()
	{
		return heeftGeenHuisartsLocatie().and(heeftStatus(CervixHuisartsBerichtStatus.AANGEMAAKT));
	}
}
