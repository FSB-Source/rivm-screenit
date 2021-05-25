
package nl.rivm.screenit.model.colon.planning;

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

import java.util.Date;

import org.joda.time.DateTime;
import org.joda.time.Minutes;

public class AfspraakLocatieWrapper
{

	private Long locatieId;

	private Date startTime;

	private Date endTime;

	private transient Integer duur;

	public Long getLocatieId()
	{
		return locatieId;
	}

	public void setLocatieId(Long locatieId)
	{
		this.locatieId = locatieId;
	}

	public Date getStartTime()
	{
		return startTime;
	}

	public void setStartTime(Date startTime)
	{
		this.startTime = startTime;
	}

	public Date getEndTime()
	{
		return endTime;
	}

	public void setEndTime(Date endTime)
	{
		this.endTime = endTime;
	}

	public Integer getDuur()
	{
		if (duur == null)
		{
			duur = Math.abs(Minutes.minutesBetween(new DateTime(startTime), new DateTime(endTime)).getMinutes());
		}
		return duur;
	}

	public void setDuur(Integer duur)
	{
		this.duur = duur;
	}

}
