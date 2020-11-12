package nl.rivm.screenit.dto.mamma;

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

public class MammaUploadBeeldenVerzoekDto implements Serializable
{
	private Long ziekenhuisId;

	private String instellingNaam;

	private Long aantalOpenstaand;

	private String telefoon;

	private String telefoon2;

	public String getInstellingNaam()
	{
		return instellingNaam;
	}

	public void setInstellingNaam(String instellingNaam)
	{
		this.instellingNaam = instellingNaam;
	}

	public Long getAantalOpenstaand()
	{
		return aantalOpenstaand;
	}

	public void setAantalOpenstaand(Long aantalOpenstaand)
	{
		this.aantalOpenstaand = aantalOpenstaand;
	}

	public Long getZiekenhuisId()
	{
		return ziekenhuisId;
	}

	public void setZiekenhuisId(Long ziekenhuisId)
	{
		this.ziekenhuisId = ziekenhuisId;
	}

	public String getTelefoon()
	{
		return telefoon;
	}

	public void setTelefoon(String telefoon)
	{
		this.telefoon = telefoon;
	}

	public String getTelefoon2()
	{
		return telefoon2;
	}

	public void setTelefoon2(String telefoon2)
	{
		this.telefoon2 = telefoon2;
	}
}
