package nl.rivm.screenit.mamma.se.dto.actions;

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

import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;

public class OnderzoekStartenDto extends AbstractActionDto
{

	private long afspraakId;

	private Boolean dubbeleTijd;

	private MammaAmputatie amputatie;

	public OnderzoekStartenDto()
	{
		super(SEActieType.ONDERZOEK_STARTEN);
	}

	public long getAfspraakId()
	{
		return afspraakId;
	}

	public void setAfspraakId(long newValue)
	{
		afspraakId = newValue;
	}

	public Boolean getDubbeleTijd()
	{
		return dubbeleTijd;
	}

	public void setDubbeleTijd(Boolean dubbeleTijd)
	{
		this.dubbeleTijd = dubbeleTijd;
	}

	public MammaAmputatie getAmputatie()
	{
		return amputatie;
	}

	public void setAmputatie(MammaAmputatie amputatie)
	{
		this.amputatie = amputatie;
	}
}
