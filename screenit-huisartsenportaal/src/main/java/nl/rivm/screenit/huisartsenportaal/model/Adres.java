package nl.rivm.screenit.huisartsenportaal.model;

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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;

import org.hibernate.envers.Audited;

@Entity
@Audited
public class Adres extends AbstractReferenceObject
{
	private static final long serialVersionUID = 1L;

	@Column(length = 43)
	private String straat;

	private Integer huisnummer;

	@Column(length = 26)
	private String huisnummertoevoeging;

	@Column(length = 12)
	private String postcode;

	@ManyToOne(fetch = FetchType.EAGER)
	private Woonplaats woonplaats;

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

	public Woonplaats getWoonplaats()
	{
		return woonplaats;
	}

	public void setWoonplaats(Woonplaats woonplaats)
	{
		this.woonplaats = woonplaats;
	}
}
