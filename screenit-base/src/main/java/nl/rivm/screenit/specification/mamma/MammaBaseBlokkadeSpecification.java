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

import java.time.LocalDate;
import java.util.Date;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaBlokkade_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaats_;
import nl.rivm.screenit.model.mamma.enums.MammaBlokkadeType;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.util.Pair;

import com.google.common.collect.BoundType;
import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.RangeSpecification.overlapt;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaBaseBlokkadeSpecification
{
	public static Specification<MammaBlokkade> getBlokkades()
	{
		return (r, q, cb) ->
		{
			var screeningsEenheidJoin = joinBlokkadeNaarScreeningsEenheid(r);
			var standPlaatsJoin = join(r, MammaBlokkade_.standplaats, JoinType.LEFT);
			var regioJoin = join(r, MammaBlokkade_.regio, JoinType.LEFT);

			return cb.or(
				cb.isTrue(screeningsEenheidJoin.get(MammaScreeningsEenheid_.actief)),
				cb.isTrue(standPlaatsJoin.get(MammaStandplaats_.actief)),
				cb.isTrue(regioJoin.get(Instelling_.actief))
			);
		};
	}

	public static Specification<MammaBlokkade> isActief()
	{
		return isActief(true);
	}

	public static Specification<MammaBlokkade> isActief(Boolean actief)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaBlokkade_.actief), actief);
	}

	public static Specification<MammaBlokkade> filterOpActief(Boolean actief)
	{
		return skipWhenNull(actief, isActief(actief));
	}

	public static Specification<MammaBlokkade> isGeldigOp(LocalDate dag)
	{
		return bevat(r -> r.get(MammaBlokkade_.vanaf), r -> r.get(MammaBlokkade_.totEnMet), Pair.of(BoundType.CLOSED, BoundType.CLOSED),
			DateUtil.toUtilDate(dag));
	}

	public static Specification<MammaBlokkade> heeftScreeningsOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaBlokkade_.regio), screeningOrganisatie);
	}

	public static Specification<MammaBlokkade> filterOpStandplaats(MammaStandplaats standplaats)
	{
		return skipWhenNull(standplaats, heeftStandplaats(standplaats));
	}

	public static Specification<MammaBlokkade> heeftStandplaats(MammaStandplaats standplaats)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaBlokkade_.standplaats), standplaats);
	}

	public static Specification<MammaBlokkade> heeftScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaBlokkade_.screeningsEenheid), screeningsEenheid);
	}

	public static Specification<MammaBlokkade> filterOpScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		return skipWhenNull(screeningsEenheid, heeftScreeningsEenheid(screeningsEenheid));
	}

	public static Specification<MammaBlokkade> isNietBlokkade(MammaBlokkade blokkade)
	{
		return skipWhenNull(blokkade.getId(), (r, q, cb) -> cb.notEqual(r.get(AbstractHibernateObject_.id), blokkade.getId()));
	}

	public static Specification<MammaBlokkade> heeftOverlapMet(MammaBlokkade blokkade)
	{
		return overlapt(Range.closed(blokkade.getVanaf(), blokkade.getTotEnMet()),
			r -> r.get(MammaBlokkade_.vanaf), r -> r.get(MammaBlokkade_.totEnMet));
	}

	public static Specification<MammaBlokkade> heeftOverlapMetPeriode(Range<Date> periode)
	{
		return overlapt(periode, r -> r.get(MammaBlokkade_.vanaf), r -> r.get(MammaBlokkade_.totEnMet));
	}

	public static Specification<MammaBlokkade> filterOpRegio(ScreeningOrganisatie organisatie)
	{
		return skipWhenNull(organisatie, (r, q, cb) ->
		{
			var screeningsEenheidJoin = joinBlokkadeNaarScreeningsEenheid(r);
			var beoordelingsEenheidJoin = join(screeningsEenheidJoin, MammaScreeningsEenheid_.beoordelingsEenheid, JoinType.LEFT);
			var parentJoin = join(beoordelingsEenheidJoin, Instelling_.parent, JoinType.LEFT);
			var standPlaatsJoin = join(r, MammaBlokkade_.standplaats, JoinType.LEFT);

			return cb.or(
				cb.equal(r.get(MammaBlokkade_.regio), organisatie),
				cb.equal(parentJoin.get(Instelling_.regio), organisatie),
				cb.equal(standPlaatsJoin.get(MammaStandplaats_.regio), organisatie)
			);
		});
	}

	private static Join<MammaBlokkade, MammaScreeningsEenheid> joinBlokkadeNaarScreeningsEenheid(Root<MammaBlokkade> r)
	{
		return join(r, MammaBlokkade_.screeningsEenheid, JoinType.LEFT);
	}

	public static Specification<MammaBlokkade> filterOpTotEnMetNietVoor(LocalDate datum)
	{
		return skipWhenNull(datum, (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(MammaBlokkade_.totEnMet), DateUtil.toUtilDate(datum)));
	}

	public static Specification<MammaBlokkade> filterOpVanafNietNa(LocalDate datum)
	{
		return skipWhenNull(datum, (r, q, cb) -> cb.lessThanOrEqualTo(r.get(MammaBlokkade_.vanaf), DateUtil.toUtilDate(datum)));
	}

	public static Specification<MammaBlokkade> heeftZelfdeScreeningsEenheidRegioOfStandplaats(MammaBlokkade blokkade)
	{
		return heeftType(blokkade.getType()).and(heeftScreeningsEenheidRegioOfStandplaats(blokkade));
	}

	public static Specification<MammaBlokkade> filterOpType(MammaBlokkadeType type)
	{
		return skipWhenNull(type, heeftType(type));
	}

	private static Specification<MammaBlokkade> heeftType(MammaBlokkadeType type)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaBlokkade_.type), type);
	}

	private static Specification<MammaBlokkade> heeftScreeningsEenheidRegioOfStandplaats(MammaBlokkade blokkade)
	{
		switch (blokkade.getType())
		{
		case SCREENINGS_ORGANISATIE:
			return heeftScreeningsOrganisatie(blokkade.getRegio());
		case SCREENINGS_EENHEID:
			return heeftScreeningsEenheid(blokkade.getScreeningsEenheid());
		case STANDPLAATS:
			return heeftStandplaats(blokkade.getStandplaats());
		}
		return null;
	}
}
