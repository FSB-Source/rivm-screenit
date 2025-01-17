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

import java.math.BigDecimal;
import java.math.MathContext;

public abstract class PercentageUtil
{

	public static String percentageToString(Integer value)
	{
		return value != null ? value * 1.0 / 100.0 + "%" : "";
	}

	public static String getPercentageVanGeheel(long deel, long geheel)
	{
		BigDecimal percentage = BigDecimal.valueOf(deel)
			.divide(BigDecimal.valueOf(geheel), 4, BigDecimal.ROUND_HALF_UP)
			.multiply(BigDecimal.valueOf(100), new MathContext(4));

		return String.format("%.2f%%", percentage);
	}
}
