package nl.rivm.screenit.mamma.se.proxy.util;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DateUtil
{
	private static final Logger LOG = LoggerFactory.getLogger(DateUtil.class);

	private static final ZoneId SCREENIT_DEFAULT = ZoneId.of("Europe/Amsterdam");

	private static Duration offset = Duration.ZERO;

	private static final DateTimeFormatter DEFAULT_DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss");

	private DateUtil()
	{
	}

	public static void setOffset(Duration newOffset)
	{
		LOG.info("De tijd is gewijzigd van " + getCurrentDateTime().toString() + " naar " + ZonedDateTime.now(SCREENIT_DEFAULT).plus(newOffset).toString());
		DateUtil.offset = newOffset;
	}

	public static LocalDateTime getCurrentDateTime()
	{
		return LocalDateTime.now(SCREENIT_DEFAULT).plus(DateUtil.offset);
	}

	public static boolean isVandaag(LocalDate localDate)
	{
		return getCurrentDateTime().toLocalDate().isEqual(localDate);
	}

	public static LocalDateTime getAfgelopenMiddernacht()
	{
		return getCurrentDateTime().toLocalDate().atStartOfDay();
	}

	public static Duration getOffset()
	{
		return DateUtil.offset;
	}

	public static String format(LocalDateTime localDateTime)
	{
		return DEFAULT_DATE_TIME_FORMATTER.format(localDateTime);
	}

	public static Date toUtilDate(LocalDate localDate)
	{
		return Date.from(localDate.atStartOfDay(SCREENIT_DEFAULT).toInstant());
	}

	public static Date toUtilDate(LocalDateTime localDateTime)
	{
		return Date.from(localDateTime.atZone(SCREENIT_DEFAULT).toInstant());
	}
}
