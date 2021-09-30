package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.WeekFields;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;

import org.apache.commons.lang.StringUtils;
import org.joda.time.DateTime;
import org.joda.time.DateTimeConstants;
import org.joda.time.Days;
import org.joda.time.Interval;
import org.joda.time.Months;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class DateUtil
{
	private static final Logger LOG = LoggerFactory.getLogger(DateUtil.class);

	private static final ZoneId SCREENIT_DEFAULT = ZoneId.of("Europe/Amsterdam");

	public static DateTimeFormatter LOCAL_DATE_FORMAT = DateTimeFormatter.ofPattern(Constants.DEFAULT_DATE_FORMAT);

	public static DateTimeFormatter LOCAL_TIME_FORMAT = DateTimeFormatter.ofPattern("HH:mm");

	public static DateTimeFormatter LOCAL_TIME_FORMAT_WITH_DAY = DateTimeFormatter.ofPattern("E dd-MM-yyyy");

	public static DateTimeFormatter LOCAL_DATE_TIME_FORMAT = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm");

	public static DateTimeFormatter LOCAL_DATE_TIME_FORMAT_SECONDS = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss");

	public static DateTimeFormatter LOCAL_DATE_DAG_MAAND_FORMAT = DateTimeFormatter.ofPattern("dd-MM");

	public static DateTimeFormatter LOCAL_DATE_UITGEBREID_DAG_UITEGEBREID_MAAND_FORMAT = DateTimeFormatter.ofPattern("EEEE dd MMMM yyyy", Constants.LOCALE_NL);

	public static DateTimeFormatter LOCAL_DATE_DAG_UITGEBREID_MAAND_FORMAT = DateTimeFormatter.ofPattern("dd MMMM yyyy", Constants.LOCALE_NL);

	public static DateTimeFormatter LOCAL_DATE_TIME_WEERGAVE_CLIENTPORTAAL_FORMAT_INCL_DAG = DateTimeFormatter.ofPattern("EEEE d MMMM HH:mm", Constants.LOCALE_NL);

	public static DateTimeFormatter LOCAL_DATE_WEERGAVE_CLIENTPORTAAL_FORMAT = DateTimeFormatter.ofPattern("d MMMM yyyy", Constants.LOCALE_NL);

	private DateUtil()
	{

	}

	public static DateTime plusWerkdagen(DateTime nu, Integer werkdagen)
	{
		while (werkdagen > 0)
		{
			nu = nu.plusDays(1);
			if (nu.getDayOfWeek() != DateTimeConstants.SATURDAY && nu.getDayOfWeek() != DateTimeConstants.SUNDAY)
			{
				werkdagen--;
			}
		}
		return nu;
	}

	public static LocalDate plusWerkdagen(LocalDate datum, long aantal)
	{
		while (aantal > 0)
		{
			datum = datum.plusDays(1);
			switch (datum.getDayOfWeek())
			{
			case MONDAY:
			case TUESDAY:
			case WEDNESDAY:
			case THURSDAY:
			case FRIDAY:
				aantal--;
			}
		}
		return datum;
	}

	public static DateTime minusWerkdagen(DateTime nu, Integer werkdagen)
	{
		while (werkdagen > 0)
		{
			nu = nu.minusDays(1);
			if (nu.getDayOfWeek() != DateTimeConstants.SATURDAY && nu.getDayOfWeek() != DateTimeConstants.SUNDAY)
			{
				werkdagen--;
			}
		}
		return nu;
	}

	public static LocalDate minusWerkdagen(LocalDate datum, int aantal)
	{
		while (aantal > 0)
		{
			datum = datum.minusDays(1);
			switch (datum.getDayOfWeek())
			{
			case MONDAY:
			case TUESDAY:
			case WEDNESDAY:
			case THURSDAY:
			case FRIDAY:
				aantal--;
			}
		}
		return datum;
	}

	public static DateTime roundMinutes(DateTime startTime)
	{
		int minutes = startTime.getMinuteOfHour();
		int modulo = minutes % 5;

		if (modulo <= 2)
		{
			minutes -= modulo;
		}
		else
		{
			minutes += 5 - modulo;
		}

		if (minutes != 60)
		{
			return startTime.withMinuteOfHour(minutes);
		}

		return startTime.plusHours(1).withMinuteOfHour(0);
	}

	public static Interval roundMinutes(Interval interval)
	{
		DateTime start = interval.getStart();
		DateTime end = interval.getEnd();
		int minutes = start.getMinuteOfHour();
		int modulo = minutes % 5;

		if (modulo > 0)
		{
			minutes += 5 - modulo;
			if (minutes != 60)
			{
				start = start.withMinuteOfHour(minutes);
			}
			else
			{
				start = start.plusHours(1).withMinuteOfHour(0);
			}
		}

		minutes = end.getMinuteOfHour();
		modulo = minutes % 5;

		if (modulo > 0)
		{
			minutes -= modulo;
			end = end.withMinuteOfHour(minutes);
		}

		return new Interval(start, end);
	}

	public static List<Interval> disjunct(Interval target, Interval disjunct)
	{
		List<Interval> disjunctionResult = new ArrayList<>();
		if (target.overlaps(disjunct))
		{
			if (target.getStart().isBefore(disjunct.getStart()))
			{
				disjunctionResult.add(new Interval(target.getStart(), disjunct.getStart()));
			}
			if (target.getEnd().isAfter(disjunct.getEnd()))
			{
				disjunctionResult.add(new Interval(disjunct.getEnd(), target.getEnd()));
			}
		}
		else
		{
			disjunctionResult.add(target);
		}
		return disjunctionResult;
	}

	public static int getMonthsBetweenDates(Date startDate, Date endDate)
	{
		if (startDate.getTime() > endDate.getTime())
		{
			Date temp = startDate;
			startDate = endDate;
			endDate = temp;
		}
		DateTime start = new DateTime(startDate);
		DateTime end = new DateTime(endDate);

		Months months = Months.monthsBetween(start, end);
		if (start.plusMonths(months.getMonths()).isBefore(end))
		{
			months = months.plus(1);
		}

		return months.getMonths();

	}

	public static int getDaysBetweenIgnoreWeekends(DateTime startDate, DateTime endDate, boolean ignoreTimeOfDay)
	{

		if (startDate.equals(endDate))
		{
			return 0;
		}
		if (ignoreTimeOfDay && startDate.toLocalDate().equals(endDate.toLocalDate()))
		{
			return 0;
		}

		int dayOfWeekStartDateNumber = startDate.getDayOfWeek();

		if (dayOfWeekStartDateNumber == 6 || dayOfWeekStartDateNumber == 7)
		{
			int DaysToAdd = 8 - dayOfWeekStartDateNumber;
			startDate = startDate.plusDays(DaysToAdd);
			dayOfWeekStartDateNumber = startDate.dayOfWeek().get();
		}

		int days;
		if (ignoreTimeOfDay)
		{
			days = Days.daysBetween(startDate.toLocalDate(), endDate.toLocalDate()).getDays();
		}
		else
		{
			days = Days.daysBetween(startDate, endDate).getDays();
		}

		int weeks = days / 7;

		int excess = days % 7;

		if (excess + dayOfWeekStartDateNumber >= 6)
		{

			return weeks * 5 + excess - 2;
		}

		return weeks * 5 + excess;
	}

	public static LocalDate toLocalDate(Date utilDate)
	{
		if (utilDate == null)
		{
			return null;
		}
		if (utilDate instanceof java.sql.Date)
		{
			return ((java.sql.Date) utilDate).toLocalDate(); 
		}
		return toLocalDateTime(utilDate).toLocalDate();
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
		return LocalDateTime.ofInstant(utilDate.toInstant(), SCREENIT_DEFAULT);
	}

	public static LocalTime toLocalTime(Date utilDate)
	{
		if (utilDate == null)
		{
			return null;
		}
		if (utilDate instanceof java.sql.Time)
		{
			return ((java.sql.Time) utilDate).toLocalTime(); 
		}
		return toLocalDateTime(utilDate).toLocalTime();
	}

	public static Date toUtilDate(LocalDateTime localDateTime)
	{
		if (localDateTime == null)
		{
			return null;
		}
		ZonedDateTime atZone = localDateTime.atZone(SCREENIT_DEFAULT);
		Instant instant = atZone.toInstant();
		Date date = Date.from(instant);
		if (LOG.isTraceEnabled())
		{
			ZonedDateTime atZoneSD = localDateTime.atZone(ZoneId.systemDefault());
			Instant instantSD = atZoneSD.toInstant();
			Date dateSD = Date.from(instantSD);
			LOG.trace("Input localDateTime: " + localDateTime + " ZonedDateTime: " + atZone + " Instant: " + instant + " Date: " + date + " ZonedDateTimeSD: " + atZoneSD
				+ " InstantSD: " + instantSD + " DateSD: " + dateSD + " SD: " + ZoneId.systemDefault());
		}

		return date;
	}

	public static Date toUtilDate(LocalTime localTime, LocalDate localDate)
	{
		if (localTime == null || localDate == null)
		{
			return null;
		}
		return toUtilDate(localTime.atDate(localDate));
	}

	public static Date toUtilDate(LocalDate localDate)
	{
		if (localDate == null)
		{
			return null;
		}
		ZonedDateTime atStartOfDay = localDate.atStartOfDay(SCREENIT_DEFAULT);
		Instant instant = atStartOfDay.toInstant();
		Date date = Date.from(instant);
		if (LOG.isTraceEnabled())
		{
			ZonedDateTime atStartOfDaySD = localDate.atStartOfDay(ZoneId.systemDefault());
			Instant instantSD = atStartOfDaySD.toInstant();
			Date dateSD = Date.from(instantSD);
			LOG.trace("Input localDate: " + localDate + "ZonedDateTime: " + atStartOfDay + " Instant: " + instant + " Date: " + date + " ZonedDateTimeSD: " + atStartOfDaySD
				+ " InstantSD: " + instantSD + " DateSD: " + dateSD + " SD: " + ZoneId.systemDefault());
		}
		return date;
	}

	public static Date toUtilDateMidnight(LocalDate localDate)
	{
		return toUtilDate(localDate);
	}

	public static Date toUtilDateMidnight(LocalDateTime localDateTime)
	{
		if (localDateTime == null)
		{
			return null;
		}
		return toUtilDate(localDateTime.toLocalDate());
	}

	public static Date toUtilDateMidnight(Date date)
	{
		return toUtilDate(toLocalDate(date));
	}

	public static int getWeekNr(TemporalAccessor tempAcc)
	{
		return tempAcc.get(WeekFields.of(Constants.LOCALE_NL).weekOfWeekBasedYear());
	}

	public static Date eindDag(Date date)
	{
		return new DateTime(date).withTime(23, 59, 59, 0).toDate();
	}

	public static String formatShortDate(Date date)
	{
		LocalDate localDate = toLocalDate(date);
		if (localDate != null)
		{
			return localDate.format(LOCAL_DATE_FORMAT);
		}
		return "";
	}

	public static String formatShortDateTime(Date date)
	{
		LocalDateTime localDateTime = toLocalDateTime(date);
		if (localDateTime != null)
		{
			return localDateTime.format(LOCAL_DATE_TIME_FORMAT);
		}
		return "";
	}

	public static String formatTime(Date time)
	{
		LocalTime localTime = toLocalTime(time);
		if (localTime != null)
		{
			return localTime.format(LOCAL_TIME_FORMAT);
		}
		return "";
	}

	public static boolean isZelfdeDag(LocalDate date1, Date date2)
	{
		return DateUtil.toUtilDateMidnight(date1).compareTo(DateUtil.toUtilDateMidnight(date2)) == 0;
	}

	public static boolean isZelfdeDag(LocalDate date1, LocalDate date2)
	{
		return date1.toEpochDay() == date2.toEpochDay();
	}

	public static boolean isGeboortedatumGelijk(LocalDate geboortedatum, Client client)
	{
		GbaPersoon persoon = client.getPersoon();
		String gebDatClient = getGeboortedatum(client);
		String gebDatVergelijk = DateTimeFormatter.ofPattern(persoon.getGeboortedatumPrecisie().getDatePattern()).format(geboortedatum);
		return gebDatClient.equals(gebDatVergelijk);
	}

	public static String formatForPattern(String pattern, Date date)
	{
		DateTimeFormatter df = DateTimeFormatter.ofPattern(pattern);
		return df.format(toLocalDateTime(date));
	}

	public static Date parseDateTimeForPattern(String date, String pattern)
	{
		DateTimeFormatter df = DateTimeFormatter.ofPattern(pattern);
		return toUtilDate(LocalDateTime.parse(date, df));
	}

	public static Date parseDateForPattern(String date, String pattern)
	{
		return toUtilDate(parseLocalDateForPattern(date, pattern));
	}

	public static LocalDate parseLocalDateForPattern(String date, String pattern)
	{
		DateTimeFormatter df = DateTimeFormatter.ofPattern(pattern);
		return LocalDate.parse(date, df);
	}

	public static boolean isWithinRange(LocalDate start, LocalDate end, LocalDate testDate)
	{
		return start.toEpochDay() <= testDate.toEpochDay()
			&& testDate.toEpochDay() <= end.toEpochDay();
	}

	public static String getGeboortedatum(GbaPersoon persoon)
	{
		String geboortedatumAlsTekst = null;
		if (persoon != null && persoon.getGeboortedatum() != null)
		{
			String geboortedatumPrecisieDatePattern = Constants.DEFAULT_DATE_FORMAT;
			if (persoon.getGeboortedatumPrecisie() != null)
			{
				geboortedatumPrecisieDatePattern = persoon.getGeboortedatumPrecisie().getDatePattern();
			}
			geboortedatumAlsTekst = DateTimeFormatter.ofPattern(geboortedatumPrecisieDatePattern).format(toLocalDate(persoon.getGeboortedatum()));
		}
		return geboortedatumAlsTekst;
	}

	public static String getGeboortedatum(Client client)
	{
		if (client != null)
		{
			return getGeboortedatum(client.getPersoon());
		}
		return "";
	}

	public static boolean compareAfter(Date vergelijkingsDatum, Date referentieDatum)
	{
		return vergelijkingsDatum.compareTo(referentieDatum) > 0;
	}

	public static boolean compareBefore(Date vergelijkingsDatum, Date referentieDatum)
	{
		return vergelijkingsDatum.compareTo(referentieDatum) < 0;
	}

	public static boolean compareEquals(Date vergelijkingsDatum, Date referentieDatum)
	{
		return referentieDatum != null && vergelijkingsDatum.compareTo(referentieDatum) == 0;
	}

	public static Date zetSeconden(Date date, int seconden)
	{
		return toUtilDate(toLocalDateTime(date).withSecond(seconden));
	}

	public static String getWeergaveDatumClientportaal(LocalDateTime datum)
	{
		String weergaveDatum = datum.format(LOCAL_DATE_TIME_WEERGAVE_CLIENTPORTAAL_FORMAT_INCL_DAG);
		return StringUtils.capitalize(weergaveDatum);
	}

	public static Date minusTijdseenheid(Date datum, int hoeveelheid, ChronoUnit tijdseenheid)
	{
		return toUtilDate(toLocalDateTime(datum).minus(hoeveelheid, tijdseenheid));
	}
}
