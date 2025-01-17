package nl.rivm.screenit.specification.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.colon.IFOBTUitslag_;
import nl.rivm.screenit.specification.ExtendedSpecification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.RangeSpecification.bevat;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonFITUitslagSpecification
{
	public static ExtendedSpecification<IFOBTUitslag> heeftAnalyseDatumTussen(Range<Date> peilRange)
	{
		return bevat(peilRange, r -> r.get(IFOBTUitslag_.analyseDatum));
	}
}
