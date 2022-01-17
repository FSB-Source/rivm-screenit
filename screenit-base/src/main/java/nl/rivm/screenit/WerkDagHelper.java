
package nl.rivm.screenit;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.DayOfWeek;
import java.time.LocalDate;

import org.joda.time.DateTime;
import org.joda.time.DateTimeConstants;

public abstract class WerkDagHelper
{

	public static DateTime plusBusinessDays(DateTime dateTime, int days)
	{
		DateTime result = dateTime;
		int daysToGo = days;
		while (daysToGo > 0)
		{
			result = result.plusDays(1);
			if (result.getDayOfWeek() != DateTimeConstants.SATURDAY && result.getDayOfWeek() != DateTimeConstants.SUNDAY)
			{
				daysToGo--;
			}
		}

		return result;
	}

	public static long werkdagenTotDatum(LocalDate currentDate, LocalDate target)
	{
		long result = 0;
		while (currentDate.toEpochDay() < target.toEpochDay())
		{
			currentDate = currentDate.plusDays(1);
			if (!DayOfWeek.SATURDAY.equals(currentDate.getDayOfWeek()) && !DayOfWeek.SUNDAY.equals(currentDate.getDayOfWeek()))
			{
				result++;
			}
		}
		return result;
	}
}
