package nl.rivm.screenit.util;

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

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.WeekFields;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;

import org.apache.commons.lang.StringUtils;

import com.google.common.base.Preconditions;
import com.google.common.collect.BoundType;
import com.google.common.collect.Range;
import com.google.common.primitives.Ints;

import static nl.rivm.screenit.util.RangeUtil.range;

@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class DateUtil
{
	public static final ZoneId SCREENIT_DEFAULT_ZONE = ZoneId.of("Europe/Amsterdam");

	public static final DateTimeFormatter LOCAL_DATE_FORMAT = DateTimeFormatter.ofPattern(Constants.DEFAULT_DATE_FORMAT);

	public static final DateTimeFormatter LOCAL_TIME_FORMAT = DateTimeFormatter.ofPattern(Constants.DEFAULT_TIME_FORMAT);

	public static final DateTimeFormatter LOCAL_DATE_TIME_FORMAT = DateTimeFormatter.ofPattern(Constants.DEFAULT_DATE_FORMAT + " " + Constants.DEFAULT_TIME_FORMAT);

	public static final DateTimeFormatter LOCAL_DATE_TIME_FORMAT_DAY = DateTimeFormatter.ofPattern(Constants.DEFAULT_DATE_FORMAT_DAY + " " + Constants.DEFAULT_TIME_FORMAT,
		Constants.LOCALE_NL);

	public static final DateTimeFormatter LOCAL_DATE_UITGEBREID_DAG_UITEGEBREID_MAAND_FORMAT = DateTimeFormatter.ofPattern("EEEE dd MMMM yyyy", Constants.LOCALE_NL);

	public static final DateTimeFormatter LOCAL_DATE_DAG_UITGEBREID_MAAND_FORMAT = DateTimeFormatter.ofPattern("dd MMMM yyyy", Constants.LOCALE_NL);

	public static final DateTimeFormatter LOCAL_DATE_TIME_WEERGAVE_CLIENTPORTAAL_FORMAT_INCL_DAG = DateTimeFormatter.ofPattern("EEEE d MMMM HH:mm", Constants.LOCALE_NL);

	public static final DateTimeFormatter LOCAL_DATE_WEERGAVE_CLIENTPORTAAL_FORMAT = DateTimeFormatter.ofPattern("d MMMM yyyy", Constants.LOCALE_NL);

	public static final Date BEGIN_OF_TIME = parseDateForPattern("01-01-1900", Constants.DEFAULT_DATE_FORMAT);

	public static final Date END_OF_TIME = parseDateForPattern("01-01-4000", Constants.DEFAULT_DATE_FORMAT);

	public static Date plusWerkdagen(Date nu, Integer werkdagen)
	{
		Preconditions.checkNotNull(nu, "Nu mag niet null zijn");
		var dateTime = DateUtil.toLocalDateTime(nu);
		while (werkdagen > 0)
		{
			dateTime = dateTime.plusDays(1);
			if (isWerkdag(dateTime.toLocalDate()))
			{
				werkdagen--;
			}
		}
		return toUtilDate(dateTime);
	}

	public static boolean isWerkdag(LocalDate datum)
	{
		return datum != null && !List.of(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY).contains(datum.getDayOfWeek());
	}

	public static LocalDate plusWerkdagen(LocalDate datum, long aantal)
	{
		Preconditions.checkNotNull(datum, "Datum mag niet null zijn");

		while (aantal > 0)
		{
			datum = datum.plusDays(1);
			if (isWerkdag(datum))
			{
				aantal--;
			}
		}
		return datum;
	}

	public static LocalDate minusWerkdagen(LocalDate datum, int aantal)
	{
		Preconditions.checkNotNull(datum, "Datum mag niet null zijn");

		while (aantal > 0)
		{
			datum = datum.minusDays(1);
			if (isWerkdag(datum))
			{
				aantal--;
			}
		}
		return datum;
	}

	public static LocalDateTime roundMinutes(LocalDateTime startTime)
	{
		int minutes = startTime.getMinute();
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
			return startTime.withMinute(minutes);
		}

		return startTime.plusHours(1).withMinute(0);
	}

	public static List<Range<LocalDateTime>> disjunct(Range<LocalDateTime> target, Range<LocalDateTime> disjunct)
	{
		var disjunctionResult = new ArrayList<Range<LocalDateTime>>();
		if (overlapsLocalDateTime(target, disjunct))
		{
			if (target.lowerEndpoint().isBefore(disjunct.lowerEndpoint()))
			{
				disjunctionResult.add(Range.closed(target.lowerEndpoint(), disjunct.lowerEndpoint()));
			}
			if (target.upperEndpoint().isAfter(disjunct.upperEndpoint()))
			{
				disjunctionResult.add(Range.closed(disjunct.upperEndpoint(), target.upperEndpoint()));
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
		Preconditions.checkNotNull(startDate, "Start datum mag niet null zijn");
		Preconditions.checkNotNull(endDate, "Eind datum mag niet null zijn");

		if (startDate.getTime() > endDate.getTime())
		{
			Date temp = startDate;
			startDate = endDate;
			endDate = temp;
		}
		var start = DateUtil.toLocalDate(startDate);
		var end = DateUtil.toLocalDate(endDate);

		var months = getPeriodeTussenTweeDatums(start, end, ChronoUnit.MONTHS);

		if (start.plusMonths(months).isBefore(end))
		{
			months++;
		}

		return months;

	}

	public static int getDaysBetweenIgnoreWeekends(Date startDate, Date endDate, boolean ignoreTimeOfDay)
	{
		return getDaysBetweenIgnoreWeekends(toLocalDateTime(startDate), toLocalDateTime(endDate), ignoreTimeOfDay);
	}

	public static int getDaysBetweenIgnoreWeekends(LocalDateTime startDate, LocalDateTime endDate, boolean ignoreTimeOfDay)
	{
		Preconditions.checkNotNull(startDate, "StartDate mag niet null zijn");
		Preconditions.checkNotNull(endDate, "EndDate mag niet null zijn");

		if (startDate.equals(endDate))
		{
			return 0;
		}
		if (ignoreTimeOfDay && startDate.toLocalDate().equals(endDate.toLocalDate()))
		{
			return 0;
		}

		var dayOfWeekStartDate = startDate.getDayOfWeek();

		if (dayOfWeekStartDate == DayOfWeek.SATURDAY || dayOfWeekStartDate == DayOfWeek.SUNDAY)
		{
			int daysToAdd = 8 - dayOfWeekStartDate.getValue();
			startDate = startDate.plusDays(daysToAdd);
			dayOfWeekStartDate = startDate.getDayOfWeek();
		}

		long days;
		if (ignoreTimeOfDay)
		{
			days = ChronoUnit.DAYS.between(startDate.toLocalDate(), endDate.toLocalDate());
		}
		else
		{
			days = ChronoUnit.DAYS.between(startDate, endDate);
		}

		var weeks = days / 7;

		var excess = days % 7;

		if (excess + dayOfWeekStartDate.getValue() >= 6)
		{

			return (int) (weeks * 5 + excess - 2);
		}

		return (int) (weeks * 5 + excess);
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
		return LocalDateTime.ofInstant(utilDate.toInstant(), SCREENIT_DEFAULT_ZONE);
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

	public static Date toUtilDate(Temporal temporal)
	{
		if (temporal == null)
		{
			return null;
		}

		LocalDateTime localDateTime;
		if (temporal instanceof LocalDate)
		{
			localDateTime = ((LocalDate) temporal).atStartOfDay();
		}
		else if (temporal instanceof LocalDateTime)
		{
			localDateTime = (LocalDateTime) temporal;
		}
		else
		{
			throw new IllegalArgumentException("Unsupported Temporal type: " + temporal.getClass());
		}

		var atZone = localDateTime.atZone(SCREENIT_DEFAULT_ZONE);
		var instant = atZone.toInstant();
		var date = Date.from(instant);
		if (LOG.isTraceEnabled())
		{
			var atZoneSD = localDateTime.atZone(ZoneId.systemDefault());
			var instantSD = atZoneSD.toInstant();
			var dateSD = Date.from(instantSD);
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

	public static Date toUtilDateMidnight(LocalDate localDate)
	{
		return toUtilDate(localDate);
	}

	public static Date toUtilDateMidnight(Date date)
	{
		return toUtilDate(toLocalDate(date));
	}

	public static int getWeekNr(TemporalAccessor tempAcc)
	{
		return tempAcc.get(WeekFields.of(Constants.LOCALE_NL).weekOfWeekBasedYear());
	}

	public static Date startDag(Date date)
	{
		Preconditions.checkNotNull(date, "Datum mag niet null zijn");

		return toUtilDate(toLocalDate(date).atStartOfDay());
	}

	public static Date startMinuut(Date date)
	{
		Preconditions.checkNotNull(date, "Datum mag niet null zijn");

		return toUtilDate(toLocalDateTime(date).withSecond(0).withNano(0));
	}

	public static LocalDateTime startMinuut(LocalDateTime date)
	{
		Preconditions.checkNotNull(date, "Datum mag niet null zijn");

		return date.withSecond(0).withNano(0);
	}

	public static Date startSeconde(Date date)
	{
		Preconditions.checkNotNull(date, "Datum mag niet null zijn");

		return toUtilDate(toLocalDateTime(date).withNano(0));
	}

	public static Date eindDag(Date date)
	{
		Preconditions.checkNotNull(date, "Datum mag niet null zijn");

		return toUtilDate(toLocalDateTime(date).with(LocalTime.MAX));
	}

	public static LocalDateTime eindDag(LocalDate date)
	{
		Preconditions.checkNotNull(date, "Datum mag niet null zijn");

		return date.atTime(LocalTime.MAX);
	}

	public static String formatShortDate(Date date)
	{
		var localDate = toLocalDate(date);
		if (localDate != null)
		{
			return localDate.format(LOCAL_DATE_FORMAT);
		}
		return "";
	}

	public static String formatShortDate(LocalDateTime date)
	{
		if (date != null)
		{
			return date.format(LOCAL_DATE_FORMAT);
		}
		return "";
	}

	public static String formatShortDateTime(Date date)
	{
		var localDateTime = toLocalDateTime(date);
		if (localDateTime != null)
		{
			return localDateTime.format(LOCAL_DATE_TIME_FORMAT);
		}
		return "";
	}

	public static String formatShortDateTime(LocalDateTime date)
	{
		if (date != null)
		{
			return date.format(LOCAL_DATE_TIME_FORMAT);
		}
		return "";
	}

	public static String formatLongDateTime(LocalDateTime date)
	{
		if (date != null)
		{
			return date.format(LOCAL_DATE_TIME_FORMAT_DAY);
		}
		return "";
	}

	public static String formatTime(Date time)
	{
		var localTime = toLocalTime(time);
		if (localTime != null)
		{
			return localTime.format(LOCAL_TIME_FORMAT);
		}
		return "";
	}

	public static String formatLocalTime(LocalDateTime time)
	{
		if (time != null)
		{
			return time.format(LOCAL_TIME_FORMAT);
		}
		return "";
	}

	public static boolean isZelfdeDag(Date date1, Date date2)
	{
		return DateUtil.toUtilDateMidnight(date1).compareTo(DateUtil.toUtilDateMidnight(date2)) == 0;
	}

	public static boolean isZelfdeDag(LocalDate date1, Date date2)
	{
		return DateUtil.toUtilDateMidnight(date1).compareTo(DateUtil.toUtilDateMidnight(date2)) == 0;
	}

	public static boolean isZelfdeDag(LocalDate date1, LocalDate date2)
	{
		return DateUtil.toUtilDateMidnight(date1).compareTo(DateUtil.toUtilDateMidnight(date2)) == 0;
	}

	public static boolean isGeboortedatumGelijk(LocalDate geboortedatum, Client client)
	{
		var persoon = client.getPersoon();
		var gebDatClient = getGeboortedatum(client);
		var gebDatVergelijk = DateTimeFormatter.ofPattern(persoon.getGeboortedatumPrecisie().getDatePattern()).format(geboortedatum);
		return gebDatClient.equals(gebDatVergelijk);
	}

	public static String formatForPattern(String pattern, Date date)
	{
		var df = DateTimeFormatter.ofPattern(pattern);
		return df.format(toLocalDateTime(date));
	}

	public static Date parseDateTimeForPattern(String date, String pattern)
	{
		var df = DateTimeFormatter.ofPattern(pattern);
		return toUtilDate(LocalDateTime.parse(date, df));
	}

	public static Date parseDateForPattern(String date, String pattern)
	{
		return toUtilDate(parseLocalDateForPattern(date, pattern));
	}

	public static LocalDate parseLocalDateForPattern(String date, String pattern)
	{
		var df = DateTimeFormatter.ofPattern(pattern);
		return LocalDate.parse(date, df);
	}

	public static Date parseZonedIsoDatum(String date)
	{
		return Date.from(ZonedDateTime.parse(date, DateTimeFormatter.ISO_DATE_TIME).toInstant());
	}

	public static boolean isWithinRange(LocalDate start, LocalDate end, LocalDate testDate)
	{
		return start.toEpochDay() <= testDate.toEpochDay()
			&& testDate.toEpochDay() <= end.toEpochDay();
	}

	public static String getGeboortedatum(GbaPersoon persoon)
	{
		var geboortedatumAlsTekst = "";
		if (persoon != null && persoon.getGeboortedatum() != null)
		{
			var geboortedatumPrecisieDatePattern = Constants.DEFAULT_DATE_FORMAT;
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
		Preconditions.checkNotNull(date, "Datum mag niet null zijn");

		return toUtilDate(toLocalDateTime(date).withSecond(seconden));
	}

	public static String getWeergaveDatumClientportaal(LocalDateTime datum)
	{
		String weergaveDatum = datum.format(LOCAL_DATE_TIME_WEERGAVE_CLIENTPORTAAL_FORMAT_INCL_DAG);
		return StringUtils.capitalize(weergaveDatum);
	}

	public static Date minDagen(Date datum, int hoeveelheid)
	{
		return minusTijdseenheid(datum, hoeveelheid, ChronoUnit.DAYS);
	}

	public static Date minusTijdseenheid(Date datum, int hoeveelheid, ChronoUnit tijdseenheid)
	{
		return minusTijdseenheid(datum, (long) hoeveelheid, tijdseenheid);
	}

	public static Date minusTijdseenheid(Date datum, long hoeveelheid, ChronoUnit tijdseenheid)
	{
		Preconditions.checkNotNull(datum, "Datum mag niet null zijn");

		return toUtilDate(toLocalDateTime(datum).minus(hoeveelheid, tijdseenheid));
	}

	public static Date plusDagen(Date datum, int hoeveelheid)
	{
		return plusTijdseenheid(datum, hoeveelheid, ChronoUnit.DAYS);
	}

	public static Date plusTijdseenheid(Date datum, int hoeveelheid, ChronoUnit tijdseenheid)
	{
		return plusTijdseenheid(datum, (long) hoeveelheid, tijdseenheid);
	}

	public static Date plusTijdseenheid(Date datum, long hoeveelheid, ChronoUnit tijdseenheid)
	{
		Preconditions.checkNotNull(datum, "Datum mag niet null zijn");

		return toUtilDate(toLocalDateTime(datum).plus(hoeveelheid, tijdseenheid));
	}

	public static int getLeeftijd(LocalDate geboortedatum, LocalDate peilDatum)
	{
		return DateUtil.getPeriodeTussenTweeDatums(geboortedatum, peilDatum, ChronoUnit.YEARS);
	}

	public static int getPeriodeTussenTweeDatums(Temporal start, Temporal eind, ChronoUnit tijdseenheid)
	{
		if (start == null || eind == null)
		{
			return 0;
		}
		return Ints.checkedCast(tijdseenheid.between(start, eind));
	}

	public static boolean overlaps(Range<Date> a, Range<Date> b)
	{
		if (!a.isConnected(b))
		{
			return false;
		}

		var intersection = a.intersection(b);

		var intersects = !intersection.isEmpty();
		if (intersection.hasLowerBound() && intersection.hasUpperBound())
		{

			intersects = intersection.lowerEndpoint().equals(intersection.upperEndpoint());
		}

		return !intersects;
	}

	public static boolean overlapsLocalDateTime(Range<LocalDateTime> a, Range<LocalDateTime> b)
	{
		if (!a.isConnected(b))
		{
			return false;
		}

		var intersection = a.intersection(b);

		var intersects = !intersection.isEmpty();
		if (intersection.hasLowerBound() && intersection.hasUpperBound())
		{

			intersects = intersection.lowerEndpoint().equals(intersection.upperEndpoint());
		}

		return !intersects;
	}

	public static Range<Date> toUtilDateRange(Range<? extends Temporal> range)
	{
		var onderGrens = range.hasLowerBound() ? toUtilDate(range.lowerEndpoint()) : null;
		var onderGrensType = range.hasLowerBound() ? range.lowerBoundType() : null;
		var bovenGrens = range.hasUpperBound() ? toUtilDate(range.upperEndpoint()) : null;
		var bovenGrensType = range.hasUpperBound() ? range.upperBoundType() : null;

		return range(onderGrens, onderGrensType, bovenGrens, bovenGrensType);
	}

	public static Range<LocalDateTime> toLocalDateTimeRange(Range<LocalDate> range)
	{
		var onderGrens = range.hasLowerBound() ? range.lowerEndpoint().atStartOfDay() : null;
		var onderGrensType = range.hasLowerBound() ? range.lowerBoundType() : null;
		var bovenGrens = range.hasUpperBound() ? range.upperEndpoint().atStartOfDay() : null;
		var bovenGrensType = range.hasUpperBound() ? range.upperBoundType() : null;
		if (bovenGrensType == BoundType.CLOSED)
		{
			bovenGrens = bovenGrens.plusDays(1);
			bovenGrensType = BoundType.OPEN;
		}

		return range(onderGrens, onderGrensType, bovenGrens, bovenGrensType);
	}

}
