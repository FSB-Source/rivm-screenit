
package nl.rivm.screenit.service.impl;

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

import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Date;

import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service(value = "currentDateSuplier")
public class DefaultCurrentDateSupplier implements ICurrentDateSupplier
{

	private static final Logger LOG = LoggerFactory.getLogger(DefaultCurrentDateSupplier.class);

	private Duration offset = Duration.ZERO;

	@Override
	@Deprecated
	public DateTime getDateTime()
	{
		LocalDateTime localDateTime = getLocalDateTime();
		return new DateTime(localDateTime.getYear(), localDateTime.getMonthValue(), localDateTime.getDayOfMonth(), localDateTime.getHour(), localDateTime.getMinute(),
			localDateTime.getSecond(), localDateTime.getNano() / 1000000);
	}

	@Override
	public Date getDate()
	{
		return getDateTime().toDate();
	}

	@Override
	public Date getDateMidnight()
	{
		return getDateTime().withTimeAtStartOfDay().toDate();
	}

	@Override
	public DateTime getDateTimeMidnight()
	{
		return getDateTime().withTimeAtStartOfDay();
	}

	public Duration getOffset()
	{
		return offset;
	}

	public void setOffset(Duration offset)
	{
		LOG.debug("'Nu' gewijzigd van " + LocalDateTime.now().plus(this.offset) + " naar "
			+ LocalDateTime.now().plus(offset));

		this.offset = offset;
	}

	@Override
	public LocalDate getLocalDate()
	{
		return getLocalDateTime().toLocalDate();
	}

	@Override
	public LocalDateTime getLocalDateTime()
	{
		Duration javaTimeOffset = getOffset();
		return LocalDateTime.now().plus(javaTimeOffset);
	}

	@Override
	public LocalTime getLocalTime()
	{
		return getLocalDateTime().toLocalTime();
	}

}
