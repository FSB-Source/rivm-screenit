package nl.rivm.screenit.model.mamma.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

public enum MammaAfbeeldingZijdeDoorsnede
{
	LINKS_VERTICALE_DOORSNEDE("LinkerborstVerticaleDoorsnede", "linksVerticaleDoorsnede"),

	LINKS_HORIZONTALE_DOORSNEDE("LinkerborstHorizontaleDoorsnede", "linksHorizontaleDoorsnede"),

	RECHTS_VERTICALE_DOORSNEDE("RechterborstVerticaleDoorsnede", "rechtsVerticaleDoorsnede"),

	RECHTS_HORIZONTALE_DOORSNEDE("RechterborstHorizontaleDoorsnede", "rechtsHorizontaleDoorsnede");

	private final String svgFileName;

	private final String signalerenDoorsnedeProperty;

	MammaAfbeeldingZijdeDoorsnede(String svgFileName, String signalerenDoorsnedeProperty)
	{
		this.svgFileName = svgFileName;
		this.signalerenDoorsnedeProperty = signalerenDoorsnedeProperty;
	}

	public String getSvgFileName()
	{
		return svgFileName;
	}

	public String getSignalerenDoorsnedeProperty()
	{
		return signalerenDoorsnedeProperty;
	}
}
