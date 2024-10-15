package nl.rivm.screenit.specification.algemeen;

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
import java.util.ArrayList;
import java.util.Date;

import javax.persistence.criteria.Predicate;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmptyExtended;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class PersoonSpecification
{
	public static ExtendedSpecification<GbaPersoon> isNietOverleden()
	{
		return (r, q, cb) -> cb.isNull(r.get(GbaPersoon_.overlijdensdatum));
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

	public static ExtendedSpecification<GbaPersoon> valtBinnenLeeftijdGrensRestricties(Integer minLeeftijd, Integer maxLeeftijd, Integer interval, LocalDate peildatum)
	{
		return (r, q, cb) ->
		{
			var predicates = new ArrayList<Predicate>();
			if (minLeeftijd != null)
			{
				var geboortedatumMaximaal = peildatum.minusYears(minLeeftijd);
				predicates.add(cb.lessThanOrEqualTo(r.get(GbaPersoon_.geboortedatum), DateUtil.toUtilDate(geboortedatumMaximaal)));
			}
			if (maxLeeftijd != null)
			{
				var geboortedatumMinimaal = peildatum.minusYears(maxLeeftijd + 1L);
				if (interval != null)
				{
					geboortedatumMinimaal = geboortedatumMinimaal.minusDays(interval);
				}
				predicates.add(cb.greaterThan(r.get(GbaPersoon_.geboortedatum), DateUtil.toUtilDate(geboortedatumMinimaal)));
			}
			return composePredicates(cb, predicates);
		};
	}
}
