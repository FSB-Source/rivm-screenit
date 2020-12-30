
package nl.rivm.screenit.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.model.INaam;

public enum ToegangLevel implements INaam
{

	EIGEN("Eigen data", 1),

	INSTELLING("Organisatie", 2),

	REGIO("Regio", 3),

	LANDELIJK("Landelijk", 4);

	private final String naam;

	private final int niveau;

	private ToegangLevel(String naam, int niveau)
	{
		this.naam = naam;
		this.niveau = niveau;
	}

	@Override
	public String getNaam()
	{
		return this.naam;
	}

	public int getNiveau()
	{
		return this.niveau;
	}
}
