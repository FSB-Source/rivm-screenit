package nl.rivm.screenit.service.cervix;

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

public class CervixHuisartsLocatieFilter implements Serializable
{
	private static final long serialVersionUID = 1L;

	private String achternaam;

	private String agbcode;

	private String locatieNaam;

	private String postcode;

	private String plaats;

	private String straat;

	public String getAchternaam()
	{
		return achternaam;
	}

	public void setAchternaam(String achternaam)
	{
		this.achternaam = achternaam;
	}

	public String getAgbcode()
	{
		return agbcode;
	}

	public void setAgbcode(String agbcode)
	{
		this.agbcode = agbcode;
	}

	public String getLocatieNaam()
	{
		return locatieNaam;
	}

	public void setLocatieNaam(String locatieNaam)
	{
		this.locatieNaam = locatieNaam;
	}

	public String getPostcode()
	{
		return postcode;
	}

	public void setPostcode(String postcode)
	{
		this.postcode = postcode;
	}

	public String getPlaats()
	{
		return plaats;
	}

	public void setPlaats(String plaats)
	{
		this.plaats = plaats;
	}

	public String getStraat()
	{
		return straat;
	}

	public void setStraat(String straat)
	{
		this.straat = straat;
	}
}
