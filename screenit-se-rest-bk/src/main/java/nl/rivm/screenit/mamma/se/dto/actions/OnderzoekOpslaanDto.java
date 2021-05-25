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

import nl.rivm.screenit.mamma.se.dto.onderzoek.OnderzoekSeDto;

public class OnderzoekOpslaanDto extends AbstractActionDto
{

	private long afspraakId;

	private OnderzoekSeDto onderzoek;

	public OnderzoekOpslaanDto()
	{
		super(SEActieType.ONDERZOEK_OPSLAAN);
	}

	public void setAfspraakId(long afspraakId)
	{
		this.afspraakId = afspraakId;
	}

	public long getAfspraakId()
	{
		return afspraakId;
	}

	public OnderzoekSeDto getOnderzoek()
	{
		return onderzoek;
	}

	public void setOnderzoek(OnderzoekSeDto onderzoek)
	{
		this.onderzoek = onderzoek;
	}
}
