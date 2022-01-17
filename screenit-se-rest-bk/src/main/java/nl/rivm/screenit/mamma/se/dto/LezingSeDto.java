package nl.rivm.screenit.mamma.se.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.se.dto.onderzoek.DoorsnedeAfbeeldingenSeDto;

public class LezingSeDto
{
	private String lezingType;

	private String biradsRechts;

	private String biradsLinks;

	private String radioloogNaam;

	private DoorsnedeAfbeeldingenSeDto lezingAanzichten;

	public String getLezingType()
	{
		return lezingType;
	}

	public void setLezingType(String lezingType)
	{
		this.lezingType = lezingType;
	}

	public String getBiradsRechts()
	{
		return biradsRechts;
	}

	public void setBiradsRechts(String biradsRechts)
	{
		this.biradsRechts = biradsRechts;
	}

	public String getBiradsLinks()
	{
		return biradsLinks;
	}

	public void setBiradsLinks(String biradsLinks)
	{
		this.biradsLinks = biradsLinks;
	}

	public String getRadioloogNaam()
	{
		return radioloogNaam;
	}

	public void setRadioloogNaam(String radioloogNaam)
	{
		this.radioloogNaam = radioloogNaam;
	}

	public DoorsnedeAfbeeldingenSeDto getLezingAanzichten()
	{
		return lezingAanzichten;
	}

	public void setLezingAanzichten(DoorsnedeAfbeeldingenSeDto doorsnedeAfbeeldingen)
	{
		this.lezingAanzichten = doorsnedeAfbeeldingen;
	}
}
