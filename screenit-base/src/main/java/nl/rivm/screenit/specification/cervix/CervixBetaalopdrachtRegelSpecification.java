package nl.rivm.screenit.specification.cervix;

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

import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegelSpecificatie_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegel_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel_;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixBetaalopdrachtRegelSpecification
{
	public static Specification<CervixBoekRegel> isHuisartsBetaalopdrachtRegel()
	{
		return (r, q, cb) ->
		{
			var betaalopdrachtRegelPath = r
				.get(CervixBoekRegel_.specificatie)
				.get(CervixBetaalopdrachtRegelSpecificatie_.betaalopdrachtRegel);

			return cb.isNotNull(betaalopdrachtRegelPath.get(CervixBetaalopdrachtRegel_.huisartsLocatie));
		};
	}
}
