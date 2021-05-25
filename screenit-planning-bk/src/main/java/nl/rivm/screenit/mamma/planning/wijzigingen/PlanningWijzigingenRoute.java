package nl.rivm.screenit.mamma.planning.wijzigingen;

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

import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;

import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningWeek;

public class PlanningWijzigingenRoute
{

	private final PlanningScreeningsEenheid screeningsEenheid;

	private PlanningStandplaatsPeriode vanafStandplaatsPeriode;

	private final Set<PlanningBlok> blokSet = new HashSet<>();

	private final Set<PlanningDag> dagSet = new HashSet<>();

	private final Set<PlanningWeek> weekSet = new HashSet<>();

	PlanningWijzigingenRoute(PlanningScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = screeningsEenheid;
	}

	public PlanningScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheid;
	}

	public PlanningStandplaatsPeriode getVanafStandplaatsPeriode()
	{
		return vanafStandplaatsPeriode;
	}

	public void setVanafStandplaatsPeriode(PlanningStandplaatsPeriode vanafStandplaatsPeriode)
	{
		if (this.vanafStandplaatsPeriode == null ||
			Comparator.comparing(PlanningStandplaatsPeriode::getScreeningsEenheidVolgNr).compare(this.vanafStandplaatsPeriode, vanafStandplaatsPeriode) > 0)
		{
			this.vanafStandplaatsPeriode = vanafStandplaatsPeriode;
		}
	}

	public Set<PlanningBlok> getBlokSet()
	{
		return blokSet;
	}

	public Set<PlanningDag> getDagSet()
	{
		return dagSet;
	}

	public Set<PlanningWeek> getWeekSet()
	{
		return weekSet;
	}
}
