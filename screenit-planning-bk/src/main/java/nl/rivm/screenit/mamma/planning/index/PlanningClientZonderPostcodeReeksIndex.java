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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeks;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;

public enum PlanningClientZonderPostcodeReeksIndex
{
	;

	private static NavigableMap<String, Set<PlanningClient>> clientNavigableMap = new TreeMap<>();

	private static Map<PlanningScreeningsOrganisatie, NavigableSet<String>> uncoveredPostcodesPerSoMap = new HashMap<>();

	public static void put(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		uncoveredPostcodesPerSoMap.put(screeningsOrganisatie, new TreeSet<>());
	}

	public static void putClient(PlanningClient client)
	{
		Set<PlanningClient> clienten = clientNavigableMap.computeIfAbsent(client.getPostcode(), k -> new HashSet<>());
		clienten.add(client);

		uncoveredPostcodesPerSoMap.get(client.getScreeningsOrganisatie()).add(client.getPostcode());
	}

	public static void removeClient(PlanningClient client)
	{
		Set<PlanningClient> clienten = clientNavigableMap.get(client.getPostcode());
		if (clienten != null)
		{
			clienten.remove(client);
			if (clienten.isEmpty())
			{
				clientNavigableMap.remove(client.getPostcode());
			}
		}

		uncoveredPostcodesPerSoMap.get(client.getScreeningsOrganisatie()).remove(client.getPostcode());
	}

	public static NavigableSet<String> getPostcodes(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		if (screeningsOrganisatie != null)
		{
			return uncoveredPostcodesPerSoMap.get(screeningsOrganisatie);
		}
		else
		{
			NavigableSet<String> postcodes = new TreeSet<>();
			for (NavigableSet<String> subPostcode : uncoveredPostcodesPerSoMap.values())
			{
				postcodes.addAll(subPostcode);
			}
			return postcodes;
		}
	}

	public static Collection<PlanningClient> getClienten(PlanningPostcodeReeks postcodeReeks)
	{
		Set<PlanningClient> clienten = new HashSet<>();
		for (Collection<PlanningClient> subClienten : clientNavigableMap.subMap(postcodeReeks.getVanPostcode(), true, postcodeReeks.getTotPostcode(), true).values())
		{
			clienten.addAll(subClienten);
		}
		return clienten;
	}

	public static void clear()
	{
		clientNavigableMap.clear();
		uncoveredPostcodesPerSoMap.clear();
	}
}
