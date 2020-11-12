package nl.rivm.screenit.util;

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

import java.time.LocalTime;

public class TimeRange
{
	private LocalTime vanaf;

	private LocalTime totEnMet;

	public TimeRange(LocalTime vanaf, LocalTime totEnMet)
	{
		this.vanaf = vanaf;
		this.totEnMet = totEnMet;
	}

	public boolean valtBinnen(TimeRange timeRange)
	{
		return !this.vanaf.isBefore(timeRange.getVanaf()) && !this.totEnMet.isAfter(timeRange.getTotEnMet());
	}

	public LocalTime getVanaf()
	{
		return vanaf;
	}

	public LocalTime getTotEnMet()
	{
		return totEnMet;
	}

	public void setVanaf(LocalTime vanaf)
	{
		this.vanaf = vanaf;
	}

	public void setTotEnMet(LocalTime totEnMet)
	{
		this.totEnMet = totEnMet;
	}
}
