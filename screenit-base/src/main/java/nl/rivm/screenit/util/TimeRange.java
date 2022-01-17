package nl.rivm.screenit.util;

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

import com.google.common.collect.Range;

import java.math.BigDecimal;
import java.time.Duration;
import java.time.LocalTime;

public class TimeRange
{
	private final Range<LocalTime> range;

	private TimeRange(Range<LocalTime> range)
	{
		this.range = range;
	}

	private TimeRange(LocalTime vanaf, LocalTime tot)
	{
		this.range = Range.closedOpen(vanaf, tot);
	}

	public boolean valtVolledigBinnen(TimeRange timeRange)
	{
		return timeRange != null && timeRange.range.encloses(this.range);
	}

	public boolean heeftOverlap(TimeRange timeRange)
	{
		return this.range.isConnected(timeRange.range);
	}

	public TimeRange overlappendePeriode(TimeRange timeRange)
	{
		return new TimeRange(this.range.intersection(timeRange.range));
	}

	public boolean bevat(LocalTime localTime)
	{
		return this.range.contains(localTime);
	}

	public LocalTime getVanaf()
	{
		return range.lowerEndpoint();
	}

	public LocalTime getTot()
	{
		return range.upperEndpoint();
	}

	public static TimeRange of(LocalTime vanaf, LocalTime totEnMet)
	{
		if (vanaf == null || totEnMet == null)
		{
			return null;
		}
		return new TimeRange(vanaf, totEnMet);
	}

	public BigDecimal getDurationInMinutes()
	{
		return BigDecimal.valueOf(Duration.between(getVanaf(), getTot()).toMinutes());
	}
}
