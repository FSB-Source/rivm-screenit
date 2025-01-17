package nl.rivm.screenit.util;

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

import org.jetbrains.annotations.NotNull;

import com.google.common.collect.BoundType;
import com.google.common.collect.Range;

public class RangeUtil
{
	public static <C extends Comparable<?>> @NotNull Range<C> closedOpen(C onderGrens, C bovenGrens)
	{
		Range<C> range = Range.all();
		if (onderGrens != null && bovenGrens != null)
		{
			range = Range.closedOpen(onderGrens, bovenGrens);
		}
		else if (onderGrens != null)
		{
			range = Range.atLeast(onderGrens);
		}
		else if (bovenGrens != null)
		{
			range = Range.lessThan(bovenGrens);
		}
		return range;
	}

	public static <C extends Comparable<?>> @NotNull Range<C> range(C onderGrens, BoundType onderGrensType, C bovenGrens, BoundType bovenGrensType)
	{
		Range<C> range = Range.all();
		if (onderGrens != null && onderGrensType != null && bovenGrens != null && bovenGrensType != null)
		{
			range = Range.range(onderGrens, onderGrensType, bovenGrens, bovenGrensType);
		}
		else if (onderGrens != null && onderGrensType != null)
		{
			range = Range.downTo(onderGrens, onderGrensType);
		}
		else if (bovenGrens != null && bovenGrensType != null)
		{
			range = Range.upTo(bovenGrens, bovenGrensType);
		}
		return range;
	}
}
