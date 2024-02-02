package nl.rivm.screenit.huisartsenportaal.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Date;

public class DateUtil
{

	public static final ZoneId SCREENIT_DEFAULT_ZONE = ZoneId.of("Europe/Amsterdam");

	public static String formatForPattern(String pattern, Date date)
	{
		DateTimeFormatter df = DateTimeFormatter.ofPattern(pattern);
		return df.format(toLocalDateTime(date));
	}

	public static Date toUtilDate(LocalDate localDate)
	{
		if (localDate == null)
		{
			return null;
		}
		ZonedDateTime atStartOfDay = localDate.atStartOfDay(SCREENIT_DEFAULT_ZONE);
		Instant instant = atStartOfDay.toInstant();

		return Date.from(instant);
	}

	public static Date toUtilDate(LocalDateTime localDateTime)
	{
		if (localDateTime == null)
		{
			return null;
		}
		ZonedDateTime atZone = localDateTime.atZone(SCREENIT_DEFAULT_ZONE);
		Instant instant = atZone.toInstant();

		return Date.from(instant);
	}

	public static LocalDateTime toLocalDateTime(Date utilDate)
	{
		if (utilDate == null)
		{
			return null;
		}
		if (utilDate instanceof java.sql.Timestamp)
		{
			return ((java.sql.Timestamp) utilDate).toLocalDateTime(); 
		}
		if (utilDate instanceof java.sql.Date)
		{
			return ((java.sql.Date) utilDate).toLocalDate().atStartOfDay(); 
		}
		return LocalDateTime.ofInstant(utilDate.toInstant(), SCREENIT_DEFAULT_ZONE);
	}

	public static Date minusTijdseenheid(Date datum, int hoeveelheid, ChronoUnit tijdseenheid)
	{
		return minusTijdseenheid(datum, (long) hoeveelheid, tijdseenheid);
	}

	public static Date minusTijdseenheid(Date datum, long hoeveelheid, ChronoUnit tijdseenheid)
	{
		return toUtilDate(toLocalDateTime(datum).minus(hoeveelheid, tijdseenheid));
	}

	public static Date plusTijdseenheid(Date datum, int hoeveelheid, ChronoUnit tijdseenheid)
	{
		return plusTijdseenheid(datum, (long) hoeveelheid, tijdseenheid);
	}

	public static Date plusTijdseenheid(Date datum, long hoeveelheid, ChronoUnit tijdseenheid)
	{
		return toUtilDate(toLocalDateTime(datum).plus(hoeveelheid, tijdseenheid));
	}

}
