package nl.rivm.screenit.huisartsenportaal.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-commons
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

import java.math.BigDecimal;
import java.util.Date;

public class BetalingDto extends AbstractDtoReferenceObject
{

	private VerrichtingDto verrichting;

	private BigDecimal bedrag;

	private String betalingsKenmerk;

	private Date betalingsdatum;

	private boolean debet;

	private String bedragString;

	public String getBedragString()
	{
		return bedragString;
	}

	public void setBedragString(String bedragString)
	{
		this.bedragString = bedragString;
	}

	public boolean isDebet()
	{
		return debet;
	}

	public void setDebet(boolean debet)
	{
		this.debet = debet;
	}

	public VerrichtingDto getVerrichting()
	{
		return verrichting;
	}

	public void setVerrichting(VerrichtingDto verrichting)
	{
		this.verrichting = verrichting;
	}

	public BigDecimal getBedrag()
	{
		return bedrag;
	}

	public void setBedrag(BigDecimal bedrag)
	{
		this.bedrag = bedrag;
	}

	public String getBetalingsKenmerk()
	{
		return betalingsKenmerk;
	}

	public void setBetalingsKenmerk(String betalingsKenmerk)
	{
		this.betalingsKenmerk = betalingsKenmerk;
	}

	public Date getBetalingsdatum()
	{
		return betalingsdatum;
	}

	public void setBetalingsdatum(Date betalingsdatum)
	{
		this.betalingsdatum = betalingsdatum;
	}
}
