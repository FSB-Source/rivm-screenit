package nl.rivm.screenit.mamma.se.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class MammaHuisartsDto extends SeDto
{
	private String naamHuisarts;

	private String achternaam;

	private String praktijknaam;

	private String type;

	private String postcode;

	private String straatnaam;

	private String huisnummerVolledig;

	private String plaats;

	private String weergaveNaam;

	public String getNaamHuisarts()
	{
		return naamHuisarts;
	}

	public void setNaamHuisarts(String naamHuisarts)
	{
		this.naamHuisarts = naamHuisarts;
	}

	public String getAchternaam()
	{
		return achternaam;
	}

	public void setAchternaam(String achternaam)
	{
		this.achternaam = achternaam;
	}

	public String getPraktijknaam()
	{
		return praktijknaam;
	}

	public void setPraktijknaam(String praktijknaam)
	{
		this.praktijknaam = praktijknaam;
	}

	public String getPostcode()
	{
		return postcode;
	}

	public void setPostcode(String postcode)
	{
		this.postcode = postcode;
	}

	public String getHuisnummerVolledig()
	{
		return huisnummerVolledig;
	}

	public void setHuisnummerVolledig(String huisnummerVolledig)
	{
		this.huisnummerVolledig = huisnummerVolledig;
	}

	public String getStraatnaam()
	{
		return straatnaam;
	}

	public void setStraatnaam(String straatnaam)
	{
		this.straatnaam = straatnaam;
	}

	public String getPlaats()
	{
		return plaats;
	}

	public void setPlaats(String plaats)
	{
		this.plaats = plaats;
	}

	public String getType()
	{
		return type;
	}

	public void setType(String type)
	{
		this.type = type;
	}

	public String getWeergaveNaam()
	{
		return weergaveNaam;
	}

	public void setWeergaveNaam(String weergaveNaam)
	{
		this.weergaveNaam = weergaveNaam;
	}
}
