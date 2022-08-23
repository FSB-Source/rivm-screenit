package nl.rivm.screenit.mamma.planning.service.impl;

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

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.util.NavigableSet;

import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningWeek;
import nl.rivm.screenit.mamma.planning.service.PlanningWijzigingenBepalenService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingenRoute;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class PlanningWijzigingenBepalenServiceImpl implements PlanningWijzigingenBepalenService
{
	@Override
	public void bepaalWijzigingen(PlanningDag dag)
	{
		PlanningScreeningsEenheid screeningsEenheid = dag.getScreeningsEenheid();
		PlanningStandplaatsPeriode standplaatsPeriode = dag.getStandplaatsPeriode();
		PlanningWijzigingenRoute wijzigingenRoute = PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid);
		wijzigingenRoute.getDagSet().add(dag);
		wijzigingenRoute.getWeekSet().add(dag.getWeek());
		if (standplaatsPeriode != null)
		{
			wijzigingenRoute.setVanafStandplaatsPeriode(standplaatsPeriode);
		}
		else
		{

			NavigableSet<PlanningStandplaatsPeriode> standplaatsPeriodeNavigableSet = screeningsEenheid.getStandplaatsPeriodeNavigableSet();
			if (!standplaatsPeriodeNavigableSet.isEmpty() && standplaatsPeriodeNavigableSet.last().getTotEnMet().isBefore(dag.getDatum()))
			{
				wijzigingenRoute.setVanafStandplaatsPeriode(standplaatsPeriodeNavigableSet.first());
			}
		}

		PlanningWeek herhalingsWeek = screeningsEenheid.getHerhalingsWeek();
		if (herhalingsWeek != null)
		{
			LocalDate nieuweHerhalingsWeek = dag.getDatum().plusWeeks(1).with(DayOfWeek.MONDAY);
			if (herhalingsWeek.getDatum().isBefore(nieuweHerhalingsWeek))
			{
				screeningsEenheid.setHerhalingsWeek(screeningsEenheid.getWeek(nieuweHerhalingsWeek));
			}
		}
	}
}
