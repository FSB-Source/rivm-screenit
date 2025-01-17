package nl.rivm.screenit.mamma.planning.index;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeks;

public enum PlanningPostcodeReeksIndex
{
	;

	private static final Map<Long, PlanningPostcodeReeks> postcodeReeksIdMap = new HashMap<>();

	private static final Map<String, PlanningPostcodeReeks> postcodeReeksMap = new HashMap<>();

	public static void put(PlanningPostcodeReeks postcodeReeks)
	{
		postcodeReeksIdMap.put(postcodeReeks.getId(), postcodeReeks);
		postcodeReeks.getPostcodeSet().forEach(postcode -> postcodeReeksMap.put(postcode, postcodeReeks));
	}

	public static void update(PlanningPostcodeReeks postcodeReeks, Set<String> postcodeSetOud)
	{
		Set<String> postcodeSetNieuw = postcodeReeks.getPostcodeSet();

		postcodeSetOud.removeAll(postcodeSetNieuw);
		postcodeReeksMap.keySet().removeAll(postcodeSetOud);

		postcodeSetNieuw.forEach(postcode -> postcodeReeksMap.put(postcode, postcodeReeks));
	}

	public static void remove(PlanningPostcodeReeks postcodeReeks)
	{
		postcodeReeksIdMap.remove(postcodeReeks.getId());
		postcodeReeksMap.keySet().removeAll(postcodeReeks.getPostcodeSet());
	}

	public static PlanningPostcodeReeks get(Long reeksId)
	{
		return postcodeReeksIdMap.get(reeksId);
	}

	public static PlanningPostcodeReeks get(String postcode)
	{
		return postcodeReeksMap.get(postcode);
	}

	public static void clear()
	{
		postcodeReeksIdMap.clear();
		postcodeReeksMap.clear();
	}
}
