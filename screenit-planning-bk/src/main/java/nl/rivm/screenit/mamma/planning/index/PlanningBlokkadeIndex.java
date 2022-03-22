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

import java.time.LocalDate;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import nl.rivm.screenit.mamma.planning.model.PlanningBlokkade;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;

public enum PlanningBlokkadeIndex
{
	;

	private static final Map<Long, PlanningBlokkade> blokkadeIdMap = new HashMap<>();

	private static final Map<PlanningScreeningsOrganisatie, Map<LocalDate, Set<PlanningBlokkade>>> screeningsOrganisatieBlokkadeMap = new HashMap<>();

	private static final Map<PlanningScreeningsEenheid, Map<LocalDate, Set<PlanningBlokkade>>> screeningsEenheidBlokkadeMap = new HashMap<>();

	private static final Map<PlanningStandplaats, Map<LocalDate, Set<PlanningBlokkade>>> standplaatsBlokkadeMap = new HashMap<>();

	public static void put(PlanningBlokkade blokkade)
	{
		blokkadeIdMap.put(blokkade.getId(), blokkade);

		switch (blokkade.getBlokkadeType())
		{
		case SCREENINGS_ORGANISATIE:
			PlanningScreeningsOrganisatie screeningsOrganisatie = blokkade.getScreeningsOrganisatie();

			Map<LocalDate, Set<PlanningBlokkade>> screeningsOrganisatieDatumMap = screeningsOrganisatieBlokkadeMap.computeIfAbsent(screeningsOrganisatie, k -> new HashMap<>());

			addToMap(blokkade, screeningsOrganisatieDatumMap);
			break;
		case SCREENINGS_EENHEID:
			PlanningScreeningsEenheid screeningsEenheid = blokkade.getScreeningsEenheid();

			Map<LocalDate, Set<PlanningBlokkade>> screeningsEenheidDatumMap = screeningsEenheidBlokkadeMap.computeIfAbsent(screeningsEenheid, k -> new HashMap<>());

			addToMap(blokkade, screeningsEenheidDatumMap);
			break;
		case STANDPLAATS:
			PlanningStandplaats standplaats = blokkade.getStandplaats();

			Map<LocalDate, Set<PlanningBlokkade>> standplaatsDatumMap = standplaatsBlokkadeMap.computeIfAbsent(standplaats, k -> new HashMap<>());

			addToMap(blokkade, standplaatsDatumMap);
			break;
		default:
			throw new IllegalStateException("Unexpected value: " + blokkade.getBlokkadeType());
		}
	}

	private static void addToMap(PlanningBlokkade blokkade, Map<LocalDate, Set<PlanningBlokkade>> datumMap)
	{
		for (LocalDate date = blokkade.getVanaf(); date.compareTo(blokkade.getTotEnMet()) <= 0; date = date.plusDays(1))
		{
			Set<PlanningBlokkade> blokkadeSet = datumMap.computeIfAbsent(date, k -> new HashSet<>());
			blokkadeSet.add(blokkade);
		}
	}

	public static void remove(PlanningBlokkade blokkade)
	{
		blokkadeIdMap.remove(blokkade.getId());
		switch (blokkade.getBlokkadeType())
		{
		case SCREENINGS_ORGANISATIE:
			PlanningScreeningsOrganisatie screeningsOrganisatie = blokkade.getScreeningsOrganisatie();

			Map<LocalDate, Set<PlanningBlokkade>> screeningsOrganisatieDatumMap = screeningsOrganisatieBlokkadeMap.get(screeningsOrganisatie);
			removeFromMap(blokkade, screeningsOrganisatieDatumMap);

			if (screeningsOrganisatieDatumMap.isEmpty())
			{
				screeningsOrganisatieBlokkadeMap.remove(screeningsOrganisatie);
			}
			break;
		case SCREENINGS_EENHEID:
			PlanningScreeningsEenheid screeningsEenheid = blokkade.getScreeningsEenheid();

			Map<LocalDate, Set<PlanningBlokkade>> screeningsEenheidDatumMap = screeningsEenheidBlokkadeMap.get(screeningsEenheid);
			removeFromMap(blokkade, screeningsEenheidDatumMap);

			if (screeningsEenheidDatumMap.isEmpty())
			{
				screeningsEenheidBlokkadeMap.remove(screeningsEenheid);
			}
			break;
		case STANDPLAATS:
			PlanningStandplaats standplaats = blokkade.getStandplaats();

			Map<LocalDate, Set<PlanningBlokkade>> standplaatsDatumMap = standplaatsBlokkadeMap.get(standplaats);
			removeFromMap(blokkade, standplaatsDatumMap);

			if (standplaatsDatumMap.isEmpty())
			{
				standplaatsBlokkadeMap.remove(standplaats);
			}
			break;
		}
	}

	private static void removeFromMap(PlanningBlokkade blokkade, Map<LocalDate, Set<PlanningBlokkade>> datumMap)
	{
		for (LocalDate date = blokkade.getVanaf(); date.compareTo(blokkade.getTotEnMet()) <= 0; date = date.plusDays(1))
		{
			Set<PlanningBlokkade> blokkadeSet = datumMap.get(date);
			blokkadeSet.remove(blokkade);
			if (blokkadeSet.isEmpty())
			{
				datumMap.remove(date);
			}
		}
	}

	public static PlanningBlokkade get(Long id)
	{
		return blokkadeIdMap.get(id);
	}

	public static Map<LocalDate, Set<PlanningBlokkade>> getBlokkadeDatumMap(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		return screeningsOrganisatieBlokkadeMap.get(screeningsOrganisatie);
	}

	public static Map<LocalDate, Set<PlanningBlokkade>> getBlokkadeDatumMap(PlanningScreeningsEenheid screeningsEenheid)
	{
		return screeningsEenheidBlokkadeMap.get(screeningsEenheid);
	}

	public static Map<LocalDate, Set<PlanningBlokkade>> getBlokkadeDatumMap(PlanningStandplaats standplaats)
	{
		return standplaatsBlokkadeMap.get(standplaats);
	}

	public static void clear()
	{
		standplaatsBlokkadeMap.clear();
		screeningsEenheidBlokkadeMap.clear();
		blokkadeIdMap.clear();
		screeningsOrganisatieBlokkadeMap.clear();
	}

}
