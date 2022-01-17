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

import com.fasterxml.jackson.databind.annotation.JsonSerialize;

@JsonSerialize
public class AanvraagStatistiekenDto
{

	public Integer aantalVerstuurd;

	public Integer aantalNogTeVersturen;

	public Integer aantalTotaal;

	public Integer getAantalVerstuurd()
	{
		return aantalVerstuurd;
	}

	public void setAantalVerstuurd(Integer aantalVerstuurd)
	{
		this.aantalVerstuurd = aantalVerstuurd;
	}

	public Integer getAantalNogTeVersturen()
	{
		return aantalNogTeVersturen;
	}

	public void setAantalNogTeVersturen(Integer aantalNogTeVersturen)
	{
		this.aantalNogTeVersturen = aantalNogTeVersturen;
	}

	public Integer getAantalTotaal()
	{
		return aantalTotaal;
	}

	public void setAantalTotaal(Integer aantalTotaal)
	{
		this.aantalTotaal = aantalTotaal;
	}
}
