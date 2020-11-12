
package nl.rivm.screenit.model.colon;

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

import java.io.Serializable;
import java.util.Date;

public class RoosterListViewFilter implements Serializable, Cloneable
{
	
	private static final long serialVersionUID = 1L;

	private RoosterItemStatus status;

	private Date startDatum;

	private Date endDatum;

	private boolean rekeningHoudenMetCapaciteitMeeBepaald = true;

	public void resetFilter()
	{
		setStatus(null);
		startDatum = null;
		endDatum = null;
		rekeningHoudenMetCapaciteitMeeBepaald = true;
	}

	public Date getStartDatum()
	{
		return startDatum;
	}

	public void setStartDatum(Date startDatum)
	{
		this.startDatum = startDatum;
	}

	public Date getEndDatum()
	{
		return endDatum;
	}

	public void setEndDatum(Date endDatum)
	{
		this.endDatum = endDatum;
	}

	public RoosterItemStatus getStatus()
	{
		return status;
	}

	public void setStatus(RoosterItemStatus status)
	{
		this.status = status;
	}

	public boolean getRekeningHoudenMetCapaciteitMeeBepaald()
	{
		return rekeningHoudenMetCapaciteitMeeBepaald;
	}

	public void setRekeningHoudenMetCapaciteitMeeBepaald(boolean rekeningHoudenMetCapaciteitMeeBepaald)
	{
		this.rekeningHoudenMetCapaciteitMeeBepaald = rekeningHoudenMetCapaciteitMeeBepaald;
	}

	@Override
	public RoosterListViewFilter clone()
	{
		try
		{
			return (RoosterListViewFilter) super.clone();
		}
		catch (CloneNotSupportedException e)
		{
			return null;
		}
	}
}
