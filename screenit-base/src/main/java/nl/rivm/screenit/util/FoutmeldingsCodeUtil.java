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

import java.text.DecimalFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class FoutmeldingsCodeUtil
{
	private static Long errorCount = 0L;

	private static LocalDate lastError = LocalDate.now();

	private static final DecimalFormat ERR_NO_FORMAT = new DecimalFormat("000000");

	public static synchronized String getFoutmeldingsCode(String prefix)
	{
		LocalDate currError = LocalDate.now();
		if (!lastError.equals(LocalDate.now()))
		{
			errorCount = 0L;
		}
		lastError = currError;

		if (prefix.length() > 0)
		{
			prefix += "_";
		}

		return prefix + currError.format(DateTimeFormatter.ofPattern("YYYY_MM_dd")) + "_" + ERR_NO_FORMAT.format(errorCount++);
	}
}
