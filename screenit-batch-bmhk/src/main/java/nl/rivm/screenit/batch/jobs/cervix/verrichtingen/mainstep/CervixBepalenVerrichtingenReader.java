package nl.rivm.screenit.batch.jobs.cervix.verrichtingen.mainstep;

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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.specification.cervix.CervixMonsterSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.cervix.verrichtingen.CervixBepalenVerrichtingenJobConfiguration.CERVIX_BEPALEN_VERRICHTINGEN_JOB_FETCH_SIZE;

@Component
public class CervixBepalenVerrichtingenReader extends BaseSpecificationScrollableResultReader<CervixMonster, Long>
{
	public CervixBepalenVerrichtingenReader()
	{
		super.setFetchSize(CERVIX_BEPALEN_VERRICHTINGEN_JOB_FETCH_SIZE);
	}

	public Specification<CervixMonster> createSpecification()
	{
		return CervixMonsterSpecification.heeftBrief()
			.and(CervixMonsterSpecification.heeftGeenVerrichtingen())
			.and(
				CervixMonsterSpecification.isOntvangenUitstrijkje()
					.and(CervixMonsterSpecification.heeftGeenOntvangenHuisartsBericht())
					.or(CervixMonsterSpecification.isZas())
			);
	}
}
