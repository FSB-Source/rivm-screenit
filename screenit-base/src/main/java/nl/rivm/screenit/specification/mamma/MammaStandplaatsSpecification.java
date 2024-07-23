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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.util.DateUtil.toUtilDate;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaStandplaatsSpecification
{
	public static Specification<MammaStandplaatsPeriode> heeftStandplaatsOpOfNaDatum(MammaStandplaats standplaats, LocalDate afsprakenVanafDatum)
	{
		return (r, q, cb) ->
		{
			var rondeJoin = join(r, MammaStandplaatsPeriode_.standplaatsRonde);
			return cb.and(
				cb.equal(rondeJoin.get(MammaStandplaatsRonde_.standplaats), standplaats),
				cb.greaterThanOrEqualTo(r.get(MammaStandplaatsPeriode_.totEnMet), toUtilDate(afsprakenVanafDatum))
			);
		};
	}
}
