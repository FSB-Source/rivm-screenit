package nl.rivm.screenit.model.project;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import lombok.Getter;

import nl.rivm.screenit.model.INaam;

@Getter
public enum GroepInvoer implements INaam
{
	CRITERIA("Groep selecteren door Criteria"),

	IMPORT("Groep selecteren door importeren", GroepSelectieType.STATISCH),

	;

	private final String naam;

	private final GroepSelectieType[] types;

	GroepInvoer(String naam, GroepSelectieType... types)
	{
		this.naam = naam;
		this.types = types;
	}

	public static List<GroepInvoer> getGroepinvoerVanSelectieType(GroepSelectieType... types)
	{
		var result = new ArrayList<GroepInvoer>();
		for (GroepInvoer invoer : GroepInvoer.values())
		{
			if (!Collections.disjoint(Arrays.asList(invoer.getTypes()), Arrays.asList(types)))
			{
				result.add(invoer);
			}
		}
		return result;
	}

}
