
package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.math.BigDecimal;

public class ColoscopieCentrumWrapper implements Serializable
{

	private static final long serialVersionUID = 1L;

	private Long id;

	private String naam;

	private String kamer;

	private String plaats;

	private BigDecimal afstand;

	public Long getId()
	{
		return id;
	}

	public void setId(Long id)
	{
		this.id = id;
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

	public BigDecimal getAfstand()
	{
		return afstand;
	}

	public void setAfstand(BigDecimal afstand)
	{
		this.afstand = afstand;
	}

	public String getKamer()
	{
		return kamer;
	}

	public void setKamer(String kamer)
	{
		this.kamer = kamer;
	}
}
