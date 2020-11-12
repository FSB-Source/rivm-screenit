
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

import nl.rivm.screenit.model.colon.planning.AfspraakStatus;

public class WerklijstIntakeFilter implements Serializable
{

	private AfspraakStatus status;

	private Date vanaf;

	private Date totEnMet;

	private ConclusieTypeFilter conclusieTypeFilter;

	private String bsn;

	private Boolean eersteKeerZoeken = true;

	public void setStatus(AfspraakStatus status)
	{
		this.status = status;
	}

	public AfspraakStatus getStatus()
	{
		return status;
	}

	public String getBsn()
	{
		return bsn;
	}

	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	public Boolean getEersteKeerZoeken()
	{
		return eersteKeerZoeken;
	}

	public void setEersteKeerZoeken(Boolean eersteKeerZoeken)
	{
		this.eersteKeerZoeken = eersteKeerZoeken;
	}

	public Date getVanaf()
	{
		return vanaf;
	}

	public void setVanaf(Date vanaf)
	{
		this.vanaf = vanaf;
	}

	public Date getTotEnMet()
	{
		return totEnMet;
	}

	public void setTotEnMet(Date totEnMet)
	{
		this.totEnMet = totEnMet;
	}

	public ConclusieTypeFilter getConclusieTypeFilter()
	{
		return conclusieTypeFilter;
	}

	public void setConclusieTypeFilter(ConclusieTypeFilter conclusieTypeFilter)
	{
		this.conclusieTypeFilter = conclusieTypeFilter;
	}
}
