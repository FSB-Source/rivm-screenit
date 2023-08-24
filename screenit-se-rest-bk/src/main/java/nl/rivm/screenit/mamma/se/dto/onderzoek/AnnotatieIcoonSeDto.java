package nl.rivm.screenit.mamma.se.dto.onderzoek;

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

import nl.rivm.screenit.mamma.se.dto.SeDto;
import nl.rivm.screenit.model.mamma.enums.MammaAnnotatieIcoonType;

public class AnnotatieIcoonSeDto extends SeDto
{
	private double positieX;

	private double positieY;

	private MammaAnnotatieIcoonType type;

	private String tekst;

	public double getPositieX()
	{
		return positieX;
	}

	public void setPositieX(double positieX)
	{
		this.positieX = positieX;
	}

	public double getPositieY()
	{
		return positieY;
	}

	public void setPositieY(double positieY)
	{
		this.positieY = positieY;
	}

	public MammaAnnotatieIcoonType getType()
	{
		return type;
	}

	public void setType(MammaAnnotatieIcoonType type)
	{
		this.type = type;
	}

	public String getTekst()
	{
		return tekst;
	}

	public void setTekst(String tekst)
	{
		this.tekst = tekst;
	}
}
