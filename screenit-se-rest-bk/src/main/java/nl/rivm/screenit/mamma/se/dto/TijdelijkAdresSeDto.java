package nl.rivm.screenit.mamma.se.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.util.Date;

public class TijdelijkAdresSeDto extends SeDto
{
	private String straat;

	private Integer huisnummer;

	private String huisletter;

	private String huisnummerToevoeging;

	private String huisnummerAanduiding;

	private String postcode;

	private String plaats;

	private Date startDatum;

	private Date eindDatum;

	public String getStraat()
	{
		return straat;
	}

	public void setStraat(String straat)
	{
		this.straat = straat;
	}

	public Integer getHuisnummer()
	{
		return huisnummer;
	}

	public void setHuisnummer(Integer huisnummer)
	{
		this.huisnummer = huisnummer;
	}

	public String getHuisletter()
	{
		return huisletter;
	}

	public void setHuisletter(String huisletter)
	{
		this.huisletter = huisletter;
	}

	public String getHuisnummerToevoeging()
	{
		return huisnummerToevoeging;
	}

	public void setHuisnummerToevoeging(String huisnummerToevoeging)
	{
		this.huisnummerToevoeging = huisnummerToevoeging;
	}

	public String getHuisnummerAanduiding()
	{
		return huisnummerAanduiding;
	}

	public void setHuisnummerAanduiding(String huisnummerAanduiding)
	{
		this.huisnummerAanduiding = huisnummerAanduiding;
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

	public Date getStartDatum()
	{
		return startDatum;
	}

	public void setStartDatum(Date startDatum)
	{
		this.startDatum = startDatum;
	}

	public Date getEindDatum()
	{
		return eindDatum;
	}

	public void setEindDatum(Date eindDatum)
	{
		this.eindDatum = eindDatum;
	}
}
