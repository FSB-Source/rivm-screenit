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

import java.util.Date;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaStandplaatsRondeSpecification
{
	public static Specification<MammaStandplaatsRonde> heeftEersteStandplaatsPeriodeVoor(Date datum)
	{
		return (r, q, cb) ->
		{
			var standplaatsPeriodenJoin = join(r, MammaStandplaatsRonde_.standplaatsPerioden);

			return cb.and(
				cb.equal(standplaatsPeriodenJoin.get(MammaStandplaatsPeriode_.standplaatsRondeVolgNr), 1),
				cb.lessThan(standplaatsPeriodenJoin.get(MammaStandplaatsPeriode_.vanaf), datum)
			);
		};
	}
}
