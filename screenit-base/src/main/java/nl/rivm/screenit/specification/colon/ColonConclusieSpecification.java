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

import java.util.Date;
import java.util.List;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonConclusie_;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.specification.ExtendedSpecification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonConclusieSpecification
{
	public static ExtendedSpecification<ColonConclusie> heeftGeenType()
	{
		return (r, q, cb) -> cb.isNull(r.get(ColonConclusie_.type));
	}

	public static ExtendedSpecification<ColonConclusie> heeftType(ColonConclusieType type)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonConclusie_.type), type);
	}

	public static ExtendedSpecification<ColonConclusie> isDoorverwijzingBevestigd(boolean bevestigd)
	{
		return (r, q, cb) -> cb.equal(r.get(ColonConclusie_.doorverwijzingBevestigd), bevestigd);
	}

	public static ExtendedSpecification<ColonConclusie> heeftTypeIn(List<ColonConclusieType> types)
	{
		return (r, q, cb) -> r.get(ColonConclusie_.type).in(types);
	}

	public static ExtendedSpecification<ColonConclusie> heeftDatumVoorOfOp(Date conclusieMoetGegevenZijnOp)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(ColonConclusie_.datum), conclusieMoetGegevenZijnOp);
	}

	public static ExtendedSpecification<ColonConclusie> heeftGeenNoShowBericht()
	{
		return (r, q, cb) -> cb.isNull(r.get(ColonConclusie_.noShowBericht));
	}
}
