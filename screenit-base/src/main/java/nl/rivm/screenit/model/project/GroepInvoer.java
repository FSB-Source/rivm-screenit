
package nl.rivm.screenit.model.project;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.CodeboekDoc;
import nl.rivm.screenit.model.INaam;

public enum GroepInvoer implements INaam
{
	@CodeboekDoc("Groep selecteren door Criteria") CRITERIA("Groep seleceteren door Criteria"),

	@CodeboekDoc("Groep selecteren door importeren") IMPORT("Groep selecteren door importeren", GroepSelectieType.STATISCH),

	;

	private String naam;

	private GroepSelectieType[] types;

	private GroepInvoer(String naam, GroepSelectieType... types)
	{
		this.naam = naam;
		this.types = types;
	}

	@Override
	public String getNaam()
	{
		return naam;
	}

	public GroepSelectieType[] getTypes()
	{
		return types;
	}

	public static List<GroepInvoer> getGroepinvoerVanSelectieType(GroepSelectieType... types)
	{
		List<GroepInvoer> invoers = new ArrayList<GroepInvoer>();
		for (GroepInvoer invoer : Arrays.asList(GroepInvoer.values()))
		{
			if (!Collections.disjoint(Arrays.asList(invoer.getTypes()), Arrays.asList(types)))
			{
				invoers.add(invoer);
			}
		}
		return invoers;
	}

}
