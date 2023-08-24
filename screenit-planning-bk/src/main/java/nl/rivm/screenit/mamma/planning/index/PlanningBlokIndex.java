package nl.rivm.screenit.mamma.planning.index;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningConceptEntiteit;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;

public enum PlanningBlokIndex
{
	;

	private static final Map<UUID, PlanningBlok> blokMap = new HashMap<>();

	private static final Map<Long, Set<PlanningBlok>> blokDeletedMap = new HashMap<>();

	private static final Map<Long, Set<PlanningBlok>> blokChangedMap = new HashMap<>();

	public static void put(PlanningBlok blok)
	{
		blokMap.put(blok.getConceptId(), blok);
	}

	public static PlanningBlok get(UUID conceptId)
	{
		return blokMap.get(conceptId);
	}

	public static void changed(PlanningScreeningsEenheid screeningsEenheid, Collection<PlanningBlok> blokSet)
	{
		Long screeningsEenheidId = screeningsEenheid.getId();

		Set<PlanningBlok> blokChangedSet = blokChangedMap.computeIfAbsent(screeningsEenheidId, k -> new HashSet<>());
		blokChangedSet.addAll(blokSet);
	}

	public static void deleted(PlanningScreeningsEenheid screeningsEenheid, Collection<PlanningBlok> blokSet)
	{
		Long screeningsEenheidId = screeningsEenheid.getId();

		Set<PlanningBlok> blokDeletedSet = blokDeletedMap.computeIfAbsent(screeningsEenheidId, k -> new HashSet<>());
		blokDeletedSet.addAll(blokSet);

		Set<PlanningBlok> blokChangedSet = blokChangedMap.get(screeningsEenheidId);
		if (blokChangedSet != null)
		{
			blokChangedSet.removeAll(blokSet);
		}

		blokSet.stream().map(PlanningConceptEntiteit::getConceptId).collect(Collectors.toList()).forEach(blokMap.keySet()::remove);
	}

	public static Set<PlanningBlok> getBlokDeletedSet(PlanningScreeningsEenheid screeningsEenheid)
	{
		Set<PlanningBlok> deletedBlokSet = blokDeletedMap.get(screeningsEenheid.getId());
		return deletedBlokSet == null ? Collections.emptySet() : deletedBlokSet;
	}

	public static Set<PlanningBlok> getBlokChangedSet(PlanningScreeningsEenheid screeningsEenheid)
	{
		Set<PlanningBlok> changedBlokSet = blokChangedMap.get(screeningsEenheid.getId());
		return changedBlokSet == null ? Collections.emptySet() : changedBlokSet;
	}

	public static void removeAll()
	{
		blokMap.clear();
		blokDeletedMap.clear();
		blokChangedMap.clear();
	}

	public static void removeAll(PlanningScreeningsEenheid screeningsEenheid)
	{
		for (PlanningBlok blok : screeningsEenheid.getBlokSet())
		{
			blokMap.remove(blok.getConceptId());
		}
		blokDeletedMap.remove(screeningsEenheid.getId());
		blokChangedMap.remove(screeningsEenheid.getId());
	}

	public static void reset(PlanningScreeningsEenheid screeningsEenheid)
	{
		blokDeletedMap.remove(screeningsEenheid.getId());
		blokChangedMap.remove(screeningsEenheid.getId());
	}

}
