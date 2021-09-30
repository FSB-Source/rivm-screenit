package nl.rivm.screenit.huisartsenportaal.dto.locatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;

public class LocatieResultDto
{

	private long aantalLocaties;

	private List<LocatieDto> locaties = new ArrayList<LocatieDto>();

	public long getAantalLocaties()
	{
		return aantalLocaties;
	}

	public void setAantalLocaties(long aantalLocaties)
	{
		this.aantalLocaties = aantalLocaties;
	}

	public List<LocatieDto> getLocaties()
	{
		return locaties;
	}

	public void setLocaties(List<LocatieDto> locaties)
	{
		this.locaties = locaties;
	}
}
