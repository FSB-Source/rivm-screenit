package nl.rivm.screenit.mamma.se.dto.onderzoek;

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

import nl.rivm.screenit.mamma.se.dto.SeDto;

public class SignalerenSeDto extends SeDto
{
	private boolean heeftAfwijkingen;

	private DoorsnedeAfbeeldingenSeDto doorsnedeAfbeeldingen;

	public boolean getHeeftAfwijkingen()
	{
		return heeftAfwijkingen;
	}

	public void setHeeftAfwijkingen(boolean heeftAfwijkingen)
	{
		this.heeftAfwijkingen = heeftAfwijkingen;
	}

	public DoorsnedeAfbeeldingenSeDto getDoorsnedeAfbeeldingen()
	{
		return doorsnedeAfbeeldingen;
	}

	public void setDoorsnedeAfbeeldingen(DoorsnedeAfbeeldingenSeDto doorsnedeAfbeeldingen)
	{
		this.doorsnedeAfbeeldingen = doorsnedeAfbeeldingen;
	}
}
