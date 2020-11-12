package nl.rivm.screenit.mamma.se.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import nl.rivm.screenit.model.mamma.MammaMammograaf;

public class MammograafDto extends SeDto
{

	private String werkstationIpAdres;

	private String aeTitle;

	private long screeningsEenheidId;

	public MammograafDto(MammaMammograaf entity)
	{
		setId(entity.getId());
		werkstationIpAdres = entity.getWerkstationIpAdres();
		aeTitle = entity.getAeTitle();
		screeningsEenheidId = entity.getScreeningsEenheid().getId();
	}

	public String getWerkstationIpAdres()
	{
		return werkstationIpAdres;
	}

	public void setWerkstationIpAdres(String werkstationIpAdres)
	{
		this.werkstationIpAdres = werkstationIpAdres;
	}

	public String getAeTitle()
	{
		return aeTitle;
	}

	public void setAeTitle(String aeTitle)
	{
		this.aeTitle = aeTitle;
	}

	public long getScreeningsEenheidId()
	{
		return screeningsEenheidId;
	}

	public void setScreeningsEenheidId(long screeningsEenheidId)
	{
		this.screeningsEenheidId = screeningsEenheidId;
	}
}
