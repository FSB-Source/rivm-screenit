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

import java.util.Date;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;
import static nl.rivm.screenit.specification.cervix.CervixBoekRegelSpecification.persoonJoin;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class PersoonSpecification
{
	public static PathAwarePredicate<GbaPersoon> isNietVertrokkenUitNederlandVoorDatumPredicate(Date datum)
	{
		return (cb, r) ->
			cb.or(
				cb.isNull(r.get(GbaPersoon_.datumVertrokkenUitNederland)),
				cb.greaterThan(r.get(GbaPersoon_.datumVertrokkenUitNederland), datum)
			);
	}

	public static PathAwarePredicate<GbaPersoon> isNietOverledenVoorPredicate(Date datum)
	{
		return (cb, r) ->
			cb.or(
				cb.isNull(r.get(GbaPersoon_.overlijdensdatum)),
				cb.greaterThan(r.get(GbaPersoon_.overlijdensdatum), datum)
			);
	}

	public static PathAwarePredicate<GbaPersoon> heeftGeenOverledenDatumPredicate()
	{
		return (cb, r) -> cb.isNull(r.get(GbaPersoon_.overlijdensdatum));
	}

	public static PathAwarePredicate<GbaPersoon> heeftGeenVertrokkenUitNederlandDatumPredicate()
	{
		return (cb, r) -> cb.isNull(r.get(GbaPersoon_.datumVertrokkenUitNederland));
	}

	public static Specification<CervixBoekRegel> filterGeboortedatum(Date geboortedatum)
	{
		return skipWhenNull(geboortedatum, (r, q, cb) -> cb.equal(persoonJoin(r).get(GbaPersoon_.geboortedatum), geboortedatum));
	}

	public static Specification<CervixBoekRegel> filterBsn(String bsn)
	{
		return SpecificationUtil.skipWhenEmpty(bsn, (r, q, cb) -> cb.equal(persoonJoin(r).get(GbaPersoon_.bsn), bsn));
	}
}
