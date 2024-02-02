package nl.rivm.screenit.model.mamma.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

public enum MammaMeldingNiveau
{

	INFO("icon-green", "Groen"),

	WAARSCHUWING("icon-orange", "Oranje"),

	PROBLEEM("icon-red", "Rood");

	private final String cssClass;

	private final String indicatieTekst;

	MammaMeldingNiveau(String cssClass, String indicatieTekst)
	{
		this.cssClass = cssClass;
		this.indicatieTekst = indicatieTekst;
	}

	public String getCssClass()
	{
		return cssClass;
	}

	public String getIndicatieTekst()
	{
		return indicatieTekst;
	}
}
