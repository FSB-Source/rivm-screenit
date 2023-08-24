package nl.rivm.screenit.mamma.planning.controller;

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

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
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
import nl.rivm.screenit.util.DateUtil;

import com.google.common.collect.Range;

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
		Date viewStartDateTime;
		Date viewEndTime;

		if (blok.conceptId != null)
		{
			viewStartDateTime = DateUtil.startDag(blok.vanaf);
			viewEndTime = DateUtil.plusDagen(viewStartDateTime, 1);
		}
		else
		{
			viewStartDateTime = blok.vanaf;
			viewEndTime = blok.tot;
		}

		var currentViewRange = Range.closed(DateUtil.startMinuut(viewStartDateTime), DateUtil.startMinuut(viewEndTime));

		var nieuweBlokken = getNieuweBlokken(blok, currentViewRange);

		List<Object[]> overlapteBlokken = null;
		if (wijzigen || blok.conceptId == null) 
		{
			overlapteBlokken = getBlokTijden(nieuweBlokken, blok);
		}

		if (blok.conceptId == null) 
		{
			if (!overlapteBlokken.isEmpty())
			{

				throw new SeTijdBlokOverlapException(getProps().getProperty("blok.heeft.overlap"), overlapteBlokken, blok.screeningsEenheidId);
			}
		}
		else if (wijzigen)
		{
			List<Range<Date>> echteOverlapteBlokken = new ArrayList<>();

			Set<UUID> idsVanBestaandeBlokken = new HashSet<>();
			idsVanBestaandeBlokken.add(blok.conceptId);

			for (Object[] overlapteBlokItem : overlapteBlokken)
			{
				var startDateTimeBestaand = DateUtil.startMinuut((Date) overlapteBlokItem[0]);
				var endDateTimeBestaand = DateUtil.startMinuut((Date) overlapteBlokItem[1]);
				var overlapteBlok = Range.closed(startDateTimeBestaand, endDateTimeBestaand);
				UUID blokItemId = (UUID) overlapteBlokItem[2];

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

	private static List<Object[]> getBlokTijden(List<Range<Date>> nieuweBlokken, PlanningCapaciteitBlokDto blokDto)
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
				for (var rangeToCheck : nieuweBlokken)
				{
					var tot = blok.getDateTot();
					var vanaf = blok.getDateVanaf();
					var range = Range.closed(vanaf, tot);
					if (DateUtil.overlaps(rangeToCheck, range))
					{
						if (aantalMammografen == null || (aantalMammografen == 1
							|| aantalMammografen > 1 && (blokDto.blokType.equals(MammaCapaciteitBlokType.GEEN_SCREENING) || blokDto.blokType.equals(blok.getCapaciteitBlokType())
							|| blok.getCapaciteitBlokType().equals(MammaCapaciteitBlokType.GEEN_SCREENING))))
						{
							Object[] items = new Object[] { vanaf, tot, blok.getConceptId() };
							overlappendeBlokken.add(items);
						}
					}
				}
			}
		}

		return overlappendeBlokken;
	}

	private static List<Range<Date>> getNieuweBlokken(PlanningCapaciteitBlokDto blok, Range<Date> currentViewRange)
	{
		List<Range<Date>> nieuweBlokken = new ArrayList<>();
		addNieuwBlok(blok, currentViewRange, nieuweBlokken);
		return nieuweBlokken;
	}

	private static void addNieuwBlok(PlanningCapaciteitBlokDto blok, Range<Date> currentViewRange, List<Range<Date>> nieuweBlokken)
	{
		var nieuwBlok = Range.closed(DateUtil.startMinuut(blok.vanaf), DateUtil.startMinuut(blok.tot));
		if (DateUtil.overlaps(currentViewRange, nieuwBlok) && !nieuweBlokken.contains(nieuwBlok))
		{
			nieuweBlokken.add(nieuwBlok);
		}
	}

}
