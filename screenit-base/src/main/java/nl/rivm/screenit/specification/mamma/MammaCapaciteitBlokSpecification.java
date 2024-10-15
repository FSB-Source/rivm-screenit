package nl.rivm.screenit.specification.mamma;

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

import java.util.Collection;
import java.util.Date;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaBlokkade_;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.RangeSpecification.overlapt;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaCapaciteitBlokSpecification
{
	public static Specification<MammaCapaciteitBlok> voorScreeningsEenheidInPeriode(MammaScreeningsEenheid screeningsEenheid,
		Collection<MammaCapaciteitBlokType> blokTypes, Range<Date> periode)
	{
		return heeftScreeningsEenheid(screeningsEenheid)
			.and(heeftOverlapMetPeriode(periode))
			.and(filterOpBlokTypes(blokTypes));
	}

	public static Specification<MammaCapaciteitBlok> heeftScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaCapaciteitBlok_.screeningsEenheid), screeningsEenheid);
	}

	public static Specification<MammaCapaciteitBlok> heeftScreeningsEenheidId(long screeningsEenheidId)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaCapaciteitBlok_.screeningsEenheid).get(AbstractHibernateObject_.id), screeningsEenheidId);
	}

	public static Specification<MammaCapaciteitBlok> heeftOverlapMetPeriode(Range<Date> periode)
	{
		return overlapt(periode, r -> r.get(MammaCapaciteitBlok_.vanaf), r -> r.get(MammaCapaciteitBlok_.tot));
	}

	public static Specification<MammaCapaciteitBlok> filterOpBlokTypes(Collection<MammaCapaciteitBlokType> blokTypes)
	{
		return skipWhenNull(blokTypes, (r, q, cb) -> r.get(MammaCapaciteitBlok_.blokType).in(blokTypes));
	}

	public static Specification<MammaCapaciteitBlok> metActieveAfspraken()
	{
		return (r, q, cb) ->
		{
			var afspraak = join(r, MammaCapaciteitBlok_.afspraken, JoinType.LEFT);
			var uitnodiging = join(afspraak, MammaAfspraak_.uitnodiging, JoinType.LEFT);
			return cb.or(cb.isNull(afspraak.get(AbstractHibernateObject_.id)),
				cb.equal(uitnodiging.get(MammaUitnodiging_.laatsteAfspraak), afspraak));
		};
	}

	public static Specification<MammaCapaciteitBlok> heeftGeenActieveBlokkade(Range<Date> zoekPeriode, MammaScreeningsEenheid screeningsEenheid,
		ScreeningOrganisatie screeningOrganisatie, MammaStandplaats standplaats)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(MammaBlokkade.class);
			subquery.select(subRoot.get(AbstractHibernateObject_.id));

			subquery.where(cb.and(MammaBaseBlokkadeSpecification.isActief()
					.and(MammaBaseBlokkadeSpecification.heeftOverlapMetPeriode(zoekPeriode)) 
					.and(blokkadeBevatCapaciteitBlok(r))
					.and(MammaBaseBlokkadeSpecification.heeftScreeningsEenheid(screeningsEenheid)
						.or(MammaBaseBlokkadeSpecification.heeftScreeningsOrganisatie(screeningOrganisatie))
						.or(MammaBaseBlokkadeSpecification.filterOpStandplaats(standplaats)))
					.toPredicate(subRoot, q, cb)
				)
			);
			return cb.not(cb.exists(subquery));
		};
	}

	private static Specification<MammaBlokkade> blokkadeBevatCapaciteitBlok(Root<MammaCapaciteitBlok> capaciteitBlokRoot)
	{
		return (r, q, cb) -> cb.and(
			cb.lessThanOrEqualTo(atStartOfDay(cb, r.get(MammaBlokkade_.vanaf)),
				atStartOfDay(cb, capaciteitBlokRoot.get(MammaCapaciteitBlok_.tot))),
			cb.greaterThanOrEqualTo(atStartOfDay(cb, r.get(MammaBlokkade_.totEnMet)),
				atStartOfDay(cb, capaciteitBlokRoot.get(MammaCapaciteitBlok_.tot)))
		);
	}

	private static Expression<Date> atStartOfDay(CriteriaBuilder cb, Path<Date> datePath)
	{
		return cb.function("date", Date.class, datePath);
	}
}
