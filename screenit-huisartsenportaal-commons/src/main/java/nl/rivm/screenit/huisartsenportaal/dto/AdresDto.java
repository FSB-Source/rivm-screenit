package nl.rivm.screenit.huisartsenportaal.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-commons
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;

@JsonSerialize
public class AdresDto extends AbstractDtoReferenceObject
{
	@NotNull(message = "Straat is verplicht.")
	@Size(max = 43, message = "Straatnaam is te lang.")
	private String straat;

	@NotNull(message = "Huisnummer is verplicht.")
	private Integer huisnummer;

	@Size(max = 26, message = "Huisnummer toevoeging is te lang.")
	private String huisnummertoevoeging;

	@NotNull(message = "Postcode is verplicht.")
	@Size(max = 6, message = "Postcode is te lang.")
	@Pattern(regexp = "\\A[1-9][0-9]{3}[ ]?([A-RT-Za-rt-z][A-Za-z]|[sS][BCbcE-Re-rT-Zt-z])\\z", message = "Postcode voldoet niet aan het juiste formaat.")
	private String postcode;

	@Valid
	@NotNull(message = "Woonplaats is verplicht.")
	private WoonplaatsDto woonplaats;

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

	public String getHuisnummertoevoeging()
	{
		return huisnummertoevoeging;
	}

	public void setHuisnummertoevoeging(String huisnummertoevoeging)
	{
		this.huisnummertoevoeging = huisnummertoevoeging;
	}

	public String getPostcode()
	{
		return postcode;
	}

	public void setPostcode(String postcode)
	{
		this.postcode = postcode;
	}

	public WoonplaatsDto getWoonplaats()
	{
		return woonplaats;
	}

	public void setWoonplaats(WoonplaatsDto woonplaats)
	{
		this.woonplaats = woonplaats;
	}
}
