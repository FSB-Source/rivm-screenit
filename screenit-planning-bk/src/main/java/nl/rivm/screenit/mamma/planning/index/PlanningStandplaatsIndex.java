package nl.rivm.screenit.mamma.planning.index;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;

public enum PlanningStandplaatsIndex
{
	;

	private static final Map<Long, PlanningStandplaats> standplaatsMap = new HashMap<>();

	public static PlanningStandplaats get(Long id)
	{
		return standplaatsMap.get(id);
	}

	public static void put(PlanningStandplaats standplaats)
	{
		standplaatsMap.put(standplaats.getId(), standplaats);
	}

	public static void remove(PlanningStandplaats standplaats)
	{
		standplaatsMap.remove(standplaats.getId());
	}

	public static List<Long> getStandplaatsenZonderRoute(Long screeningsOrganisatieId)
	{
		List<Long> standplaatsen = new ArrayList<>();
		for (PlanningStandplaats standplaats : standplaatsMap.values())
		{
			if (standplaats.getScreeningsOrganisatie().getId().equals(screeningsOrganisatieId)
				&& standplaats.getStandplaatsRondeNavigableSet().isEmpty())
			{
				standplaatsen.add(standplaats.getId());
			}
		}
		return standplaatsen;
	}

	public static List<Long> getStandplaatsenMetRoute(Long screeningsOrganisatieId)
	{
		List<Long> standplaatsen = new ArrayList<>();
		for (PlanningStandplaats standplaats : standplaatsMap.values())
		{
			if (standplaats.getScreeningsOrganisatie().getId().equals(screeningsOrganisatieId)
				&& !standplaats.getStandplaatsRondeNavigableSet().isEmpty())
			{
				standplaatsen.add(standplaats.getId());
			}
		}
		return standplaatsen;
	}

	public static void clear()
	{
		standplaatsMap.clear();
	}
}
