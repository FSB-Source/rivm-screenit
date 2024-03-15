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

import java.time.LocalDate;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.MergedBrieven_;
import nl.rivm.screenit.model.cervix.CervixMergedBrieven;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class CervixMergedBrievenSpecification
{
	public static PathAwarePredicate<CervixMergedBrieven> heeftPrintDatumVoorDatum(LocalDate datum)
	{
		return (cb, r) ->
			cb.lessThan(r.get(MergedBrieven_.printDatum), DateUtil.toUtilDate(datum));
	}
}
