package nl.rivm.screenit.batch.jobs.cervix.gevolgenlabprocesverwerken.step;

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
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.specification.cervix.CervixMonsterSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

@Component
public class CervixGevolgenLabprocesVerwerkenReader extends BaseSpecificationScrollableResultReader<CervixMonster>
{

	public CervixGevolgenLabprocesVerwerkenReader()
	{
		super.setFetchSize(1000);
	}

	@Override
	protected Specification<CervixMonster> createSpecification()
	{
		return CervixMonsterSpecification.heeftOntvangstRonde()
			.and(CervixMonsterSpecification.heeftGeenBrief());
	}
}
