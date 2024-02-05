package nl.rivm.screenit.specification.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie_;
import nl.rivm.screenit.model.cervix.CervixHuisarts_;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsAanmeldStatus;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenBlankPredicate;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixHuisartsSpecification
{
	public static Specification<CervixHuisarts> heeftAgbCode(String agbCode)
	{
		return (r, q, cb) -> cb.equal(r.get(CervixHuisarts_.agbcode), agbCode.trim());
	}

	public static Specification<CervixHuisarts> filterOpAgbCodeContaining(String agbCode)
	{
		return CervixHuisartsSpecification.filterOpAgbCodeContainingPredicate(agbCode).toSpecification();
	}

	public static PathAwarePredicate<CervixHuisarts> filterOpAgbCodeContainingPredicate(String agbCode)
	{
		return (cb, r) -> skipWhenBlankPredicate(agbCode, containsCaseInsensitive(cb, r.get(Instelling_.agbcode), agbCode));
	}

	public static PathAwarePredicate<CervixHuisarts> isGeregistreerd()
	{
		return (cb, r) -> cb.equal(r.get(CervixHuisarts_.aanmeldStatus), CervixHuisartsAanmeldStatus.GEREGISTREERD);
	}

	public static Specification<CervixHuisarts> heeftActieveLocatie()
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(CervixHuisartsLocatie.class);

			subquery.select(cb.count(subqueryRoot)).where(
				cb.and(
					cb.equal(subqueryRoot.get(CervixHuisartsLocatie_.status), CervixLocatieStatus.ACTIEF),
					cb.equal(subqueryRoot.get(CervixHuisartsLocatie_.huisarts).get(CervixHuisarts_.id), r.get(CervixHuisarts_.id))
				)
			);
			return cb.equal(subquery, 1);
		};
	}

	public static Specification<CervixHuisarts> isActief()
	{
		return isActiefPredicate().toSpecification();
	}

	public static PathAwarePredicate<CervixHuisarts> isActiefPredicate()
	{
		return (cb, r) -> cb.equal(r.get(Instelling_.actief), Boolean.TRUE);
	}

}
