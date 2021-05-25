package nl.rivm.screenit.dto.cervix.facturatie;

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

import java.io.Serializable;
import java.util.Date;

import nl.rivm.screenit.model.cervix.enums.CervixTariefType;

public class CervixVerrichtingenZoekObject implements Serializable
{
	private String monsterId;

	private Date geboorteDatum;

	private String bsn;

	private Date verrichtingsDatumVanaf;

	private Date verrichtingsDatumTotenmet;

	private Date datumUitstrijkje;

	private CervixTariefType verrichtingsType;

	private String betalingskenmerk;

	private boolean alleenZonderBetalingskenmerk = false;

	private Boolean debet = null;

	private boolean alleenVerrichtingen = false;

	public String getMonsterId()
	{
		return monsterId;
	}

	public void setMonsterId(String monsterId)
	{
		this.monsterId = monsterId;
	}

	public Date getGeboorteDatum()
	{
		return geboorteDatum;
	}

	public void setGeboorteDatum(Date geboorteDatum)
	{
		this.geboorteDatum = geboorteDatum;
	}

	public String getBsn()
	{
		return bsn;
	}

	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	public Date getVerrichtingsDatumVanaf()
	{
		return verrichtingsDatumVanaf;
	}

	public void setVerrichtingsDatumVanaf(Date verrichtingsDatumVanaf)
	{
		this.verrichtingsDatumVanaf = verrichtingsDatumVanaf;
	}

	public Date getVerrichtingsDatumTotenmet()
	{
		return verrichtingsDatumTotenmet;
	}

	public void setVerrichtingsDatumTotenmet(Date verrichtingsDatumTotenmet)
	{
		this.verrichtingsDatumTotenmet = verrichtingsDatumTotenmet;
	}

	public String getBetalingskenmerk()
	{
		return betalingskenmerk;
	}

	public void setBetalingskenmerk(String betalingskenmerk)
	{
		this.betalingskenmerk = betalingskenmerk;
	}

	public boolean isAlleenZonderBetalingskenmerk()
	{
		return alleenZonderBetalingskenmerk;
	}

	public void setAlleenZonderBetalingskenmerk(boolean alleenZonderBetalingskenmerk)
	{
		this.alleenZonderBetalingskenmerk = alleenZonderBetalingskenmerk;
	}

	public CervixTariefType getVerrichtingsType()
	{
		return verrichtingsType;
	}

	public void setVerrichtingsType(CervixTariefType verrichtingsType)
	{
		this.verrichtingsType = verrichtingsType;
	}

	public Date getDatumUitstrijkje()
	{
		return datumUitstrijkje;
	}

	public void setDatumUitstrijkje(Date datumUitstrijkje)
	{
		this.datumUitstrijkje = datumUitstrijkje;
	}

	public Boolean getDebet()
	{
		return debet;
	}

	public void setDebet(Boolean debet)
	{
		this.debet = debet;
	}

	public boolean isAlleenVerrichtingen()
	{
		return alleenVerrichtingen;
	}

	public void setAlleenVerrichtingen(boolean alleenVerrichtingen)
	{
		this.alleenVerrichtingen = alleenVerrichtingen;
	}
}
