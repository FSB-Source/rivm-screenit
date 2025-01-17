package nl.rivm.screenit.specification.algemeen;

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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.DateSpecification.bevatLocalDateToDate;
import static nl.rivm.screenit.specification.DateSpecification.extractYear;
import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmptyExtended;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;
import static nl.rivm.screenit.specification.algemeen.AdresSpecification.heeftEenPostcode;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class PersoonSpecification
{
	public static ExtendedSpecification<GbaPersoon> isNietOverleden()
	{
		return (r, q, cb) -> cb.isNull(r.get(GbaPersoon_.overlijdensdatum));
	}

	public static ExtendedSpecification<GbaPersoon> isOverleden()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(GbaPersoon_.overlijdensdatum));
	}

	public static ExtendedSpecification<GbaPersoon> isNietOverledenEnWoontInNederland()
	{
		return woontInNederland().and(isNietOverleden());
	}

	public static ExtendedSpecification<GbaPersoon> woontInNederland()
	{
		return (r, q, cb) -> cb.isNull(r.get(GbaPersoon_.datumVertrokkenUitNederland));
	}

	public static ExtendedSpecification<GbaPersoon> filterGeboortedatum(Date geboortedatum)
	{
		return skipWhenNullExtended(geboortedatum, (r, q, cb) -> cb.equal(r.get(GbaPersoon_.geboortedatum), geboortedatum));
	}

	public static ExtendedSpecification<GbaPersoon> filterBsn(String bsn)
	{
		return skipWhenEmptyExtended(bsn, (r, q, cb) -> cb.equal(r.get(GbaPersoon_.bsn), bsn));
	}

	public static ExtendedSpecification<GbaPersoon> filterPostcode(String postcode)
	{
		return AdresSpecification.filterPostcode(postcode).with(GbaPersoon_.gbaAdres);
	}

	public static ExtendedSpecification<GbaPersoon> filterHuisnummer(Integer huisnummer)
	{
		return AdresSpecification.filterHuisnummer(huisnummer).with(GbaPersoon_.gbaAdres);
	}

	public static ExtendedSpecification<GbaPersoon> heeftBsn(String bsn)
	{
		return (r, q, cb) -> cb.equal(r.get(GbaPersoon_.bsn), bsn);
	}

	public static ExtendedSpecification<GbaPersoon> heeftBsnIn(List<String> bsns)
	{
		return (r, q, cb) -> r.get(GbaPersoon_.bsn).in(bsns);
	}

	public static ExtendedSpecification<GbaPersoon> valtBinnenLeeftijdGrensRestricties(Integer minLeeftijd, Integer maxLeeftijd, Integer interval, LocalDate peildatum)
	{
		return (r, q, cb) ->
		{
			var predicates = new ArrayList<Predicate>();
			if (minLeeftijd != null)
			{
				var geboortedatumMaximaal = peildatum.minusYears(minLeeftijd);
				predicates.add(isGeborenVoorOfOp(geboortedatumMaximaal).toPredicate(r, q, cb));
			}
			if (maxLeeftijd != null)
			{
				var geboortedatumMinimaal = peildatum.minusYears(maxLeeftijd + 1L);
				if (interval != null)
				{
					geboortedatumMinimaal = geboortedatumMinimaal.minusDays(interval);
				}
				predicates.add(isGeborenNa(geboortedatumMinimaal).toPredicate(r, q, cb));
			}
			return composePredicates(cb, predicates);
		};
	}

	public static ExtendedSpecification<GbaPersoon> valtBinnenLeeftijd(Integer minLeeftijd, Integer maxLeeftijd, LocalDate peilDatum)
	{
		return valtBinnenLeeftijdGrensRestricties(minLeeftijd, maxLeeftijd, null, peilDatum);
	}

	public static ExtendedSpecification<GbaPersoon> isGeborenVoor(LocalDate peilDatum)
	{
		return (r, q, cb) ->
		{
			var geboorteDatum = r.get(GbaPersoon_.geboortedatum);
			return cb.lessThan(geboorteDatum, DateUtil.toUtilDate(peilDatum));
		};
	}

	public static ExtendedSpecification<GbaPersoon> isGeborenVoorOfOp(LocalDate peilDatum)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(GbaPersoon_.geboortedatum), DateUtil.toUtilDate(peilDatum));
	}

	public static ExtendedSpecification<GbaPersoon> isGeborenNa(LocalDate peilDatum)
	{
		return (r, q, cb) -> cb.greaterThan(r.get(GbaPersoon_.geboortedatum), DateUtil.toUtilDate(peilDatum));
	}

	public static ExtendedSpecification<GbaPersoon> heeftGeboorteJaarIn(List<Integer> geboorteJaren)
	{
		return skipWhenEmpty(geboorteJaren, (r, q, cb) -> extractYear(r.get(GbaPersoon_.geboortedatum), cb).in(geboorteJaren));
	}

	public static ExtendedSpecification<GbaPersoon> heeftGbaAdresMetPostcode()
	{
		return heeftEenPostcode().with(GbaPersoon_.gbaAdres)
			.or(heeftEenPostcode().with(GbaPersoon_.tijdelijkGbaAdres, JoinType.LEFT));
	}

	public static ExtendedSpecification<GbaPersoon> heeftGeboortedatumIn(Range<LocalDate> bereik)
	{
		return bevatLocalDateToDate(bereik, r -> r.get(GbaPersoon_.geboortedatum));
	}

	public static ExtendedSpecification<GbaPersoon> heeftGeslachtIn(Collection<Geslacht> geslachten)
	{
		return (r, q, cb) -> r.get(GbaPersoon_.geslacht).in(geslachten);
	}
}
