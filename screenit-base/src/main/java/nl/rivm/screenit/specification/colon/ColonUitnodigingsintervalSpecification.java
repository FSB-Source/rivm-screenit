package nl.rivm.screenit.specification.colon;

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

import java.util.List;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.colon.ColonUitnodigingsinterval;
import nl.rivm.screenit.model.colon.ColonUitnodigingsinterval_;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.specification.ExtendedSpecification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonUitnodigingsintervalSpecification
{
	public static ExtendedSpecification<ColonUitnodigingsinterval> heeftTypeIn(List<ColonUitnodigingsintervalType> types)
	{
		return (r, q, cb) -> r.get(ColonUitnodigingsinterval_.type).in(types);
	}

	public static ExtendedSpecification<ColonUitnodigingsinterval> heeftAantal()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(ColonUitnodigingsinterval_.aantal));
	}
}
