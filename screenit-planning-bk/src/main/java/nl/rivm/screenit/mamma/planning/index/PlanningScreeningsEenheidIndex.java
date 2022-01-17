package nl.rivm.screenit.mamma.planning.index;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;

public enum PlanningScreeningsEenheidIndex
{
	;

	private static final Map<Long, PlanningScreeningsEenheid> screeningsEenheidMap = new HashMap<>();

	public static PlanningScreeningsEenheid get(Long id)
	{
		return screeningsEenheidMap.get(id);
	}

	public static Collection<PlanningScreeningsEenheid> getScreeningsEenheden()
	{
		return screeningsEenheidMap.values();
	}

	public static void put(PlanningScreeningsEenheid screeningsEenheid)
	{
		screeningsEenheidMap.put(screeningsEenheid.getId(), screeningsEenheid);
	}

	public static void remove(PlanningScreeningsEenheid screeningsEenheid)
	{
		screeningsEenheidMap.remove(screeningsEenheid.getId());
	}

	public static int size()
	{
		return screeningsEenheidMap.size();
	}

	public static void clear()
	{
		screeningsEenheidMap.clear();
	}
}
