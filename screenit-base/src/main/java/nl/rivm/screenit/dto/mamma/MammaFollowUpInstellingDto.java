package nl.rivm.screenit.dto.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class MammaFollowUpInstellingDto implements Serializable
{
	private Long instellingId;

	private String instellingNaam;

	private Date laatstGebeld;

	private String telefoon;

	private String telefoon2;

	public Long getInstellingId()
	{
		return instellingId;
	}

	public void setInstellingId(Long instellingId)
	{
		this.instellingId = instellingId;
	}

	public String getInstellingNaam()
	{
		return instellingNaam;
	}

	public void setInstellingNaam(String instellingNaam)
	{
		this.instellingNaam = instellingNaam;
	}

	public Date getLaatstGebeld()
	{
		return laatstGebeld;
	}

	public void setLaatstGebeld(Date laatstGebeld)
	{
		this.laatstGebeld = laatstGebeld;
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
