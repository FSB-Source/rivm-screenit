package nl.rivm.screenit.mamma.planning.model;

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

import java.time.LocalDate;
import java.util.HashSet;
import java.util.Set;

public class PlanningDag implements Comparable<PlanningDag>
{
	private final LocalDate datum;

	private final Set<PlanningBlok> blokSet = new HashSet<>();

	private final Set<PlanningBlokkade> blokkadeSet = new HashSet<>();

	private PlanningWeek week;

	private final PlanningBeschikbaar beschikbaar = new PlanningBeschikbaar();

	private PlanningStandplaatsPeriode standplaatsPeriode;

	private final PlanningScreeningsEenheid screeningsEenheid;

	public PlanningDag(LocalDate datum, PlanningScreeningsEenheid screeningsEenheid)
	{
		this.datum = datum;
		this.screeningsEenheid = screeningsEenheid;
	}

	public PlanningWeek getWeek()
	{
		return week;
	}

	public void setWeek(PlanningWeek week)
	{
		this.week = week;
	}

	public Set<PlanningBlok> getBlokSet()
	{
		return blokSet;
	}

	public PlanningBeschikbaar getBeschikbaar()
	{
		return beschikbaar;
	}

	public PlanningStandplaatsPeriode getStandplaatsPeriode()
	{
		return standplaatsPeriode;
	}

	public void setStandplaatsPeriode(PlanningStandplaatsPeriode standplaatsPeriode)
	{
		this.standplaatsPeriode = standplaatsPeriode;
	}

	public Set<PlanningBlokkade> getBlokkadeSet()
	{
		return blokkadeSet;
	}

	public LocalDate getDatum()
	{
		return datum;
	}

	public PlanningScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheid;
	}

	@Override
	public int compareTo(PlanningDag dag)
	{
		return this.datum.compareTo(dag.datum);
	}
}
