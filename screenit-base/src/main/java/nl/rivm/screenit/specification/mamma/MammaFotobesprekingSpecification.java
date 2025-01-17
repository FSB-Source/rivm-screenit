package nl.rivm.screenit.specification.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;
import java.util.Collection;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.model.mamma.MammaFotobespreking_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingType;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftIdIn;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaFotobesprekingSpecification
{
	public static Specification<MammaFotobespreking> filterOpGestartOpVanaf(LocalDate vanaf)
	{
		return skipWhenNull(vanaf, (r, q, cb) ->
			cb.greaterThan(cb.coalesce(r.get(MammaFotobespreking_.gestartOp), DateUtil.END_OF_TIME), DateUtil.toUtilDate(vanaf)));
	}

	public static Specification<MammaFotobespreking> filterOpScreeningsEenheid(Collection<MammaScreeningsEenheid> screeningsEenheden)
	{
		return skipWhenEmpty(screeningsEenheden, (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(MammaScreeningsEenheid.class);
			var screeningsEenheidIds = screeningsEenheden.stream().map(AbstractHibernateObject::getId).toList();

			subquery.select(subqueryRoot.get(MammaScreeningsEenheid_.beoordelingsEenheid).get(SingleTableHibernateObject_.id)).distinct(true)
				.where(heeftIdIn(screeningsEenheidIds).toPredicate(subqueryRoot, q, cb));

			return cb.or(
				r.get(MammaFotobespreking_.screeningsEenheid).in(screeningsEenheidIds),
				r.get(MammaFotobespreking_.beoordelingsEenheid).in(subquery));
		});
	}

	public static Specification<MammaFotobespreking> filterOpType(Collection<MammaFotobesprekingType> types)
	{
		return skipWhenEmpty(types, (r, q, cb) -> r.get(MammaFotobespreking_.type).in(types));
	}
}
