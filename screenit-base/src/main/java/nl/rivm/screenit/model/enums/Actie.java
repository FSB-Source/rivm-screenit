
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

public enum Actie implements INaam
{
	
	INZIEN("Inzien", 1),
	
	AANPASSEN("Wijzigen", 2),
	
	TOEVOEGEN("Toevoegen", 3),
	
	VERWIJDEREN("Verwijderen", 4);

	private final String naam;

	private final int niveau;

	private Actie(String naam, int niveau)
	{
		this.naam = naam;
		this.niveau = niveau;
	}

	public String getNaam()
	{
		return this.naam;
	}

	public int getNiveau()
	{
		return this.niveau;
	}
}
