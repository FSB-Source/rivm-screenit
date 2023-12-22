package nl.rivm.screenit.specification.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;

import javax.persistence.criteria.Path;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class PersoonSpecification
{
	public static PathAwarePredicate<GbaPersoon> isNietVertrokkenUitNederlandVoorDatumPredicate(Path<Date> datum)
	{
		return (cb, r) ->
			cb.or(
				cb.isNull(r.get(GbaPersoon_.datumVertrokkenUitNederland)),
				cb.greaterThan(r.get(GbaPersoon_.datumVertrokkenUitNederland), datum)
			);
	}

	public static PathAwarePredicate<GbaPersoon> isNietOverledenVoorPredicate(Path<Date> datum)
	{
		return (cb, r) ->
			cb.or(
				cb.isNull(r.get(GbaPersoon_.overlijdensdatum)),
				cb.greaterThan(r.get(GbaPersoon_.overlijdensdatum), datum)
			);
	}
}
