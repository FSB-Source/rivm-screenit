package nl.rivm.screenit.batch.jobs.cervix.verrichtingen.dubbelecytologiestep;

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
import nl.rivm.screenit.model.cervix.CervixMonster_;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje_;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.batch.jobs.cervix.verrichtingen.CervixBepalenVerrichtingenJobConfiguration.CERVIX_BEPALEN_VERRICHTINGEN_JOB_FETCH_SIZE;
import static nl.rivm.screenit.model.cervix.enums.CervixTariefType.CYTOLOGIE_TARIEF_TYPES;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftCytologieVerslag;
import static nl.rivm.screenit.specification.cervix.CervixMonsterSpecification.heeftVerrichtingen;
import static nl.rivm.screenit.specification.cervix.CervixVerrichtingSpecification.heeftTypeIn;

@Component
public class CervixDubbeleCytologieVerrichtingenReader extends BaseSpecificationScrollableResultReader<CervixUitstrijkje>
{

	public CervixDubbeleCytologieVerrichtingenReader()
	{
		super.setFetchSize(CERVIX_BEPALEN_VERRICHTINGEN_JOB_FETCH_SIZE);
	}

	@Override
	protected Specification<CervixUitstrijkje> createSpecification()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(CervixVerrichting.class);
			subquery
				.select(subRoot.get(CervixVerrichting_.monster).get(CervixMonster_.id))
				.where(heeftTypeIn(CYTOLOGIE_TARIEF_TYPES).toPredicate(subRoot, q, cb)
				);

			return cb.and(
				cb.not(cb.in(r.get(CervixUitstrijkje_.id)).value(subquery)),
				heeftVerrichtingen().toPredicate(r, q, cb),
				heeftCytologieVerslag().toPredicate(r, q, cb)
			);
		};
	}
}
