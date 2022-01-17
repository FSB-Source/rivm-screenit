package nl.rivm.screenit.huisartsenportaal.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class VerrichtingFilterDto
{
	private String clientNaam;

	private Date verrichtingsDatumVanaf;

	private Date verrichtingsDatumTotenmet;

	private Date datumUitstrijkje;

	private LocatieDto locatie;

	private String betalingskenmerk;

	private boolean alleenZonderBetalingskenmerk = false;

	public String getClientNaam()
	{
		return clientNaam;
	}

	public void setClientNaam(String clientNaam)
	{
		this.clientNaam = clientNaam;
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

	public Date getDatumUitstrijkje()
	{
		return datumUitstrijkje;
	}

	public void setDatumUitstrijkje(Date datumUitstrijkje)
	{
		this.datumUitstrijkje = datumUitstrijkje;
	}

	public LocatieDto getLocatie()
	{
		return locatie;
	}

	public void setLocatie(LocatieDto locatie)
	{
		this.locatie = locatie;
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
}
