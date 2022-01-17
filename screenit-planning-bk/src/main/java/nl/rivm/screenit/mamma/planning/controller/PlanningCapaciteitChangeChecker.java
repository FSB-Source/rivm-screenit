package nl.rivm.screenit.mamma.planning.controller;

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

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.UUID;

import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.exceptions.SeTijdBlokOverlapException;
import nl.rivm.screenit.mamma.planning.index.PlanningBlokIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;

import org.joda.time.DateTime;
import org.joda.time.Interval;

public class PlanningCapaciteitChangeChecker
{
	private static Properties props = null;

	private static Properties getProps()
	{
		if (props == null)
		{
			props = new Properties();
			String resourceName = "capaciteitConflictMeldingen.properties"; 
			ClassLoader loader = Thread.currentThread().getContextClassLoader();
			try (InputStream resourceStream = loader.getResourceAsStream(resourceName))
			{
				props.load(resourceStream);
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		}
		return props;
	}

	public static void magCapaciteitOpslaanVerwijderen(PlanningCapaciteitBlokDto blok, boolean wijzigen) throws OpslaanVerwijderenTijdBlokException
	{
		DateTime viewStartDateTime = null;
		DateTime viewEndTime = null;

		if (blok.conceptId != null)
		{
			viewStartDateTime = new DateTime(blok.vanaf).withTimeAtStartOfDay().toDateTime();
			viewEndTime = new DateTime(viewStartDateTime).plusDays(1).withTimeAtStartOfDay().toDateTime();
		}
		else
		{
			viewStartDateTime = new DateTime(blok.vanaf);
			viewEndTime = new DateTime(blok.tot);
		}

		Interval currentViewInterval = new Interval(viewStartDateTime.withSecondOfMinute(0).withMillisOfSecond(0), viewEndTime.withSecondOfMinute(0).withMillisOfSecond(0));

		List<Interval> nieuweBlokken = getNieuweBlokken(blok, currentViewInterval);

		List<Object[]> overlapteBlokken = null;
		if (wijzigen || blok.conceptId == null) 
		{
			overlapteBlokken = getBlokTijden(nieuweBlokken, blok, currentViewInterval);
		}

		if (blok.conceptId == null) 
		{
			if (overlapteBlokken != null && !overlapteBlokken.isEmpty())
			{

				throw new SeTijdBlokOverlapException(getProps().getProperty("blok.heeft.overlap"), overlapteBlokken, blok.screeningsEenheidId);
			}
		}
		else if (wijzigen)
		{
			List<Interval> echteOverlapteBlokken = new ArrayList<>();

			Set<UUID> idsVanBestaandeBlokken = new HashSet<>();
			idsVanBestaandeBlokken.add(blok.conceptId);

			for (Object[] overlapteBlokItem : overlapteBlokken)
			{
				Object[] blokItemTijden = overlapteBlokItem;
				DateTime startDateTimeBestaand = new DateTime(blokItemTijden[0]).withSecondOfMinute(0).withMillisOfSecond(0);
				DateTime endDateTimeBestaand = new DateTime(blokItemTijden[1]).withSecondOfMinute(0).withMillisOfSecond(0);
				Interval overlapteBlok = new Interval(startDateTimeBestaand, endDateTimeBestaand);
				UUID blokItemId = (UUID) blokItemTijden[2];

				if (!idsVanBestaandeBlokken.contains(blokItemId))
				{

					echteOverlapteBlokken.add(overlapteBlok);
				}
			}
			if (echteOverlapteBlokken.size() > 0)
			{

				throw new SeTijdBlokOverlapException(getProps().getProperty("blok.heeft.overlap"), echteOverlapteBlokken, blok.screeningsEenheidId);
			}
		}
	}

	private static List<Object[]> getBlokTijden(List<Interval> nieuweBlokken, PlanningCapaciteitBlokDto blokDto, Interval currentViewInterval)
	{
		List<Object[]> overlappendeBlokken = new ArrayList<>();
		PlanningBlok changedBlok = PlanningBlokIndex.get(blokDto.conceptId);

		if (changedBlok != null)
		{
			Object[] items = new Object[] { changedBlok.getDateVanaf(), changedBlok.getDateTot(), changedBlok.getConceptId() };
			overlappendeBlokken.add(items);
		}
		PlanningScreeningsEenheid screeningsEenheid = PlanningScreeningsEenheidIndex.get(blokDto.screeningsEenheidId);
		Integer aantalMammografen = screeningsEenheid.getAantalMammografen();

		for (PlanningBlok blok : screeningsEenheid.getBlokSet())
		{
			if (!PlanningBlokIndex.getBlokDeletedSet(screeningsEenheid).contains(blok))
			{
				for (Interval intervalToCheck : nieuweBlokken)
				{
					DateTime tot = new DateTime(blok.getDateTot());
					DateTime vanaf = new DateTime(blok.getDateVanaf());
					Interval interval = new Interval(vanaf, tot);
					if (intervalToCheck.overlaps(interval))
					{
						if (aantalMammografen == null || (aantalMammografen == 1
							|| aantalMammografen > 1 && (blokDto.blokType.equals(MammaCapaciteitBlokType.GEEN_SCREENING) || blokDto.blokType.equals(blok.getCapaciteitBlokType())
								|| blok.getCapaciteitBlokType().equals(MammaCapaciteitBlokType.GEEN_SCREENING))))
						{
							Object[] items = new Object[] { vanaf.toDate(), tot.toDate(), blok.getConceptId() };
							overlappendeBlokken.add(items);
						}
					}
				}
			}
		}

		return overlappendeBlokken;
	}

	private static List<Interval> getNieuweBlokken(PlanningCapaciteitBlokDto blok, Interval currentViewInterval)
	{
		List<Interval> nieuweBlokken = new ArrayList<>();

		addNieuwBlok(blok, currentViewInterval, nieuweBlokken);
		return nieuweBlokken;
	}

	private static void addNieuwBlok(PlanningCapaciteitBlokDto blok, Interval currentViewInterval, List<Interval> nieuweBlokken)
	{
		Interval nieuwBlok = new Interval(new DateTime(blok.vanaf).withSecondOfMinute(0).withMillisOfSecond(0),
			new DateTime(blok.tot).withSecondOfMinute(0).withMillisOfSecond(0));
		if (currentViewInterval.overlaps(nieuwBlok) && !nieuweBlokken.contains(nieuwBlok))
		{
			nieuweBlokken.add(nieuwBlok);
		}
	}

}
