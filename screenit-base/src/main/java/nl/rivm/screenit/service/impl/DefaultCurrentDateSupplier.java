
package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.stereotype.Service;

@Slf4j
@Service(value = "currentDateSuplier")
public class DefaultCurrentDateSupplier implements ICurrentDateSupplier
{

	@Getter
	private Duration offset = Duration.ZERO;

	@Override
	public Date getDate()
	{
		return DateUtil.toUtilDate(getLocalDateTime());
	}

	@Override
	public Date getDateMidnight()
	{
		return DateUtil.toUtilDate(getLocalDate());
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

	public void setOffset(Duration offset)
	{
		LOG.debug("'Nu' gewijzigd van " + LocalDateTime.now().plus(this.offset) + " naar "
			+ LocalDateTime.now().plus(offset));

		this.offset = offset;
	}

}
