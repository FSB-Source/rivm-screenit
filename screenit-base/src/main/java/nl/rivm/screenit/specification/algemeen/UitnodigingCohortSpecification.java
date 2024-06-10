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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.colon.UitnodigingCohort;
import nl.rivm.screenit.model.colon.UitnodigingCohort_;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class UitnodigingCohortSpecification
{
	public static Specification<UitnodigingCohort> heeftJaarMinderOfGelijkAan(int jaar)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(UitnodigingCohort_.jaar), jaar);
	}

	public static Specification<UitnodigingCohort> heeftJaar(int jaar)
	{
		return (r, q, cb) -> cb.equal(r.get(UitnodigingCohort_.jaar), jaar);
	}
}
