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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;

public enum PlanningClientFactorTypeIndex
{
	;

	private static final Map<PlanningScreeningsOrganisatie, Map<MammaFactorType, Set<PlanningClient>>> screeningsOrganisatieClientenPerFactorTypeMap = new HashMap<>();

	public static Set<PlanningClient> get(PlanningScreeningsOrganisatie screeningsOrganisatie, MammaFactorType factorType)
	{
		Map<MammaFactorType, Set<PlanningClient>> clientenPerFactorType = screeningsOrganisatieClientenPerFactorTypeMap.get(screeningsOrganisatie);
		return clientenPerFactorType != null ? clientenPerFactorType.get(factorType) : null;
	}

	public static void put(PlanningClient client)
	{
		PlanningScreeningsOrganisatie screeningsOrganisatie = client.getScreeningsOrganisatie();
		Map<MammaFactorType, Set<PlanningClient>> factorTypeClientMap = screeningsOrganisatieClientenPerFactorTypeMap.get(screeningsOrganisatie);

		if (factorTypeClientMap == null)
		{
			factorTypeClientMap = new HashMap<>();
			for (MammaFactorType factorType : MammaFactorType.values())
			{
				factorTypeClientMap.put(factorType, new HashSet<>());
			}
			screeningsOrganisatieClientenPerFactorTypeMap.put(screeningsOrganisatie, factorTypeClientMap);
		}

		putInMap(client, factorTypeClientMap);
	}

	private static void putInMap(PlanningClient client, Map<MammaFactorType, Set<PlanningClient>> factorTypeClientMap)
	{
		MammaFactorType factorType = client.getFactorType();
		Set<PlanningClient> clientSet = factorTypeClientMap.get(factorType);
		clientSet.add(client);
	}

	public static void clear()
	{
		screeningsOrganisatieClientenPerFactorTypeMap.clear();
	}
}
