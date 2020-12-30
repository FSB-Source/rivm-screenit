
package nl.rivm.screenit.model.colon;

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

public class ColoscopieCentrumZoekCriteria implements Serializable
{

	private static final long serialVersionUID = 1L;

	private String naam;

	private String plaats;

	private Integer afstand;

	public ColoscopieCentrumZoekCriteria()
	{
	}

	public String getNaam()
	{
		return naam;
	}

	public void setNaam(String naam)
	{
		this.naam = naam;
	}

	public String getPlaats()
	{
		return plaats;
	}

	public void setPlaats(String plaats)
	{
		this.plaats = plaats;
	}

	public Integer getAfstand()
	{
		return afstand;
	}

	public void setAfstand(Integer afstand)
	{
		this.afstand = afstand;
	}

}
