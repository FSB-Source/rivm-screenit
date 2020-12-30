
package nl.rivm.screenit.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Calendar;

import org.joda.time.DateTimeConstants;

public enum Dag
{

	ZONDAG(Calendar.SUNDAY, DateTimeConstants.SUNDAY),

	MAANDAG(Calendar.MONDAY, DateTimeConstants.MONDAY),

	DINSDAG(Calendar.TUESDAY, DateTimeConstants.TUESDAY),

	WOENSDAG(Calendar.WEDNESDAY, DateTimeConstants.WEDNESDAY),

	DONDERDAG(Calendar.THURSDAY, DateTimeConstants.THURSDAY),

	VRIJDAG(Calendar.FRIDAY, DateTimeConstants.FRIDAY),

	ZATERDAG(Calendar.SATURDAY, DateTimeConstants.SATURDAY);

	private int calendarDay;

	private int jodaDay;

	private Dag(int calendarDay, int jodaDay)
	{
		this.calendarDay = calendarDay;
		this.jodaDay = jodaDay;
	}

	public int getCalendarDay()
	{
		return calendarDay;
	}

	public int getJodaDay()
	{
		return jodaDay;
	}

	public static Dag getDagByJodaDay(int jodaDay)
	{
		for (Dag dag : Dag.values())
		{
			if (dag.getJodaDay() == jodaDay)
			{
				return dag;
			}
		}

		return null;
	}
}
