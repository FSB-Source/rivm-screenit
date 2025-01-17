package nl.rivm.screenit.mamma.planning.wijzigingen;

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
import java.util.NavigableSet;
import java.util.Set;

import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeks;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeksRegio;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.model.PlanningTehuis;

public enum PlanningWijzigingen
{
	;

	private static final Set<PlanningClient> clientSet = new HashSet<>();

	private static final Set<PlanningPostcodeReeksRegio> postcodeReeksRegioSet = new HashSet<>();

	private static final Set<PlanningPostcodeReeks> postcodeReeksSet = new HashSet<>();

	private static final Set<PlanningTehuis> tehuisSet = new HashSet<>();

	private static final Set<PlanningStandplaats> standplaatsSet = new HashSet<>();

	private static final Map<PlanningScreeningsEenheid, PlanningWijzigingenRoute> wijzigingenRouteMap = new HashMap<>();

	public static PlanningWijzigingenRoute getWijzigingenRoute(PlanningScreeningsEenheid screeningsEenheid)
	{
		PlanningWijzigingenRoute wijzigingenRoute = wijzigingenRouteMap.get(screeningsEenheid);
		if (wijzigingenRoute == null)
		{
			wijzigingenRoute = new PlanningWijzigingenRoute(screeningsEenheid);
			wijzigingenRouteMap.put(screeningsEenheid, wijzigingenRoute);
		}
		return wijzigingenRoute;
	}

	public static void bepaalWijzigingen(PlanningStandplaats standplaats)
	{
		getStandplaatsSet().add(standplaats);

		for (PlanningStandplaatsRonde standplaatsRonde : standplaats.getStandplaatsRondeNavigableSet())
		{
			for (PlanningStandplaatsPeriode standplaatsPeriode : standplaatsRonde.getStandplaatsPeriodeNavigableSet())
			{
				getWijzigingenRoute(standplaatsPeriode.getScreeningsEenheid()).setVanafStandplaatsPeriode(standplaatsPeriode);
			}
		}
	}

	static Collection<PlanningWijzigingenRoute> getWijzigingenRoutes()
	{
		bepaalGewijzigdeRoutes();
		return wijzigingenRouteMap.values();
	}

	public static Set<PlanningClient> getClientSet()
	{
		return clientSet;
	}

	public static Set<PlanningPostcodeReeksRegio> getPostcodeReeksRegioSet()
	{
		return postcodeReeksRegioSet;
	}

	public static Set<PlanningPostcodeReeks> getPostcodeReeksSet()
	{
		return postcodeReeksSet;
	}

	public static Set<PlanningTehuis> getTehuisSet()
	{
		return tehuisSet;
	}

	public static Set<PlanningStandplaats> getStandplaatsSet()
	{
		return standplaatsSet;
	}

	private static void bepaalGewijzigdeRoutes()
	{
		Map<PlanningScreeningsEenheid, PlanningWijzigingenRoute> origineleWijzigingenRouteMap = new HashMap<>(wijzigingenRouteMap);

		origineleWijzigingenRouteMap.forEach((screeningsEenheid, wijzigingenRoute) ->
		{
			if (wijzigingenRoute.getVanafStandplaatsPeriode() != null)
			{
				NavigableSet<PlanningStandplaatsPeriode> standplaatsPeriodeNavigableSet = screeningsEenheid.getStandplaatsPeriodeNavigableSet().tailSet(
					wijzigingenRoute.getVanafStandplaatsPeriode(),
					true);

				for (PlanningStandplaatsPeriode standplaatsPeriode : standplaatsPeriodeNavigableSet)
				{
					NavigableSet<PlanningStandplaatsPeriode> gesplitsteStandplaatsPeriodeNavigableSet = standplaatsPeriode.getStandplaatsRonde().getStandplaatsPeriodeNavigableSet()
						.tailSet(standplaatsPeriode, false);

					gesplitsteStandplaatsPeriodeNavigableSet.forEach(gesplitsteStandplaatsPeriode -> getWijzigingenRoute(gesplitsteStandplaatsPeriode.getScreeningsEenheid())
						.setVanafStandplaatsPeriode(gesplitsteStandplaatsPeriode));
				}
			}
		});
	}

	public static void clear()
	{
		clientSet.clear();
		postcodeReeksRegioSet.clear();
		postcodeReeksSet.clear();
		tehuisSet.clear();
		standplaatsSet.clear();
		wijzigingenRouteMap.clear();
	}
}
