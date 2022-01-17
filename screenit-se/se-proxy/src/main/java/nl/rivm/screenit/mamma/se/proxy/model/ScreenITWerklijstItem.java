package nl.rivm.screenit.mamma.se.proxy.model;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

public abstract class ScreenITWerklijstItem
{
	private String aeTitle; 

	private String medewerkercode;

	ScreenITWerklijstItem()
	{
	}

	ScreenITWerklijstItem(String medewerkercode)
	{
		this.medewerkercode = medewerkercode;
	}

	public String getAeTitle()
	{
		return aeTitle;
	}

	public void setAeTitle(String aeTitle)
	{
		this.aeTitle = aeTitle;
	}

	public String getMedewerkercode()
	{
		return medewerkercode;
	}

	public void setMedewerkercode(String medewerkercode)
	{
		this.medewerkercode = medewerkercode;
	}
}
