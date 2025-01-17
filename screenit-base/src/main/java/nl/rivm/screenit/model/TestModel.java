package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.enums.ColonTest;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

public class TestModel implements Serializable
{

	private static final long serialVersionUID = 1L;

	private ColonTest colonTestActies;

	private String bsn;

	private Date geboortedatum;

	private Geslacht geslacht;

	private Date datumOverlijden;

	private Gemeente gemeente;

	private GbaStatus gbaStatus = GbaStatus.INDICATIE_AANWEZIG;

	public String getBsn()
	{
		return bsn;
	}

	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	public Date getGeboortedatum()
	{
		return geboortedatum;
	}

	public void setGeboortedatum(Date geboortedatum)
	{
		this.geboortedatum = geboortedatum;
	}

	public Gemeente getGemeente()
	{
		return gemeente;
	}

	public void setGemeente(Gemeente gemeente)
	{
		this.gemeente = gemeente;
	}

	public ColonTest getColonTestActies()
	{
		return colonTestActies;
	}

	public void setTestActie(ColonTest colonTestActies)
	{
		this.colonTestActies = colonTestActies;
	}

	public GbaStatus getGbaStatus()
	{
		return gbaStatus;
	}

	public void setGbaStatus(GbaStatus gbaStatus)
	{
		this.gbaStatus = gbaStatus;
	}

	public Date getDatumOverlijden()
	{
		return datumOverlijden;
	}

	public void setDatumOverlijden(Date datumOverlijden)
	{
		this.datumOverlijden = datumOverlijden;
	}

	public Geslacht getGeslacht()
	{
		return geslacht;
	}

	public void setGeslacht(Geslacht geslacht)
	{
		this.geslacht = geslacht;
	}

}
