package nl.rivm.screenit.mamma.planning.index;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;

public enum PlanningScreeningsOrganisatieIndex
{
	;

	private static final Map<Long, PlanningScreeningsOrganisatie> screeningsOrganisatieMap = new HashMap();

	public static Collection<PlanningScreeningsOrganisatie> getScreeningsOrganisaties()
	{
		return screeningsOrganisatieMap.values();
	}

	public static void put(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		screeningsOrganisatieMap.put(screeningsOrganisatie.getId(), screeningsOrganisatie);
		PlanningClientZonderPostcodeReeksIndex.put(screeningsOrganisatie);
	}

	public static PlanningScreeningsOrganisatie get(Long id)
	{
		return screeningsOrganisatieMap.get(id);
	}

	public static void clear()
	{
		screeningsOrganisatieMap.clear();
	}
}
