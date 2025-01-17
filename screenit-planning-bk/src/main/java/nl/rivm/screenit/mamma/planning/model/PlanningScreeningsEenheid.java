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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;
import nl.rivm.screenit.util.DateUtil;

import static java.time.DayOfWeek.MONDAY;

public class PlanningScreeningsEenheid extends PlanningEntiteit
{
	private PlanningScreeningsOrganisatie screeningsOrganisatie;

	private final NavigableSet<PlanningStandplaatsPeriode> standplaatsPeriodeNavigableSet = new TreeSet<>(
		Comparator.comparing(PlanningStandplaatsPeriode::getScreeningsEenheidVolgNr));

	private final Set<PlanningBlok> blokSet = new HashSet<>();

	private final NavigableMap<LocalDate, PlanningDag> dagNavigableMap = new TreeMap<>();

	private final NavigableMap<LocalDate, PlanningWeek> weekNavigableMap = new TreeMap<>();

	private final LocalDate uitgenodigdTotEnMet;

	private final LocalDate uitnodigenTotEnMet;

	private PlanningWeek initieelHerhalingsWeek;

	private PlanningWeek herhalingsWeek;

	private Integer aantalMammografen;

	private MammaMeldingNiveau niveau = MammaMeldingNiveau.INFO;

	private BigDecimal initieelInterval;

	private BigDecimal interval;

	private int volgNrOffset;

	public PlanningScreeningsEenheid(Long id, LocalDate uitgenodigdTotEnMet, LocalDate uitnodigenTotEnMet, BigDecimal initieelInterval, LocalDate herhalingsWeekMaandag)
	{
		super(id);
		this.uitgenodigdTotEnMet = uitgenodigdTotEnMet;
		this.uitnodigenTotEnMet = uitnodigenTotEnMet;
		this.initieelInterval = initieelInterval;

		blokSet.clear();
		dagNavigableMap.clear();
		weekNavigableMap.clear();
		for (LocalDate date = PlanningConstanten.plannenVanafDatum; date.compareTo(PlanningConstanten.plannenTotEnMetDatum) <= 0; date = date.plusDays(1))
		{
			var dag = new PlanningDag(date, this);
			putDag(date, dag);

			LocalDate weekDag = date.with(MONDAY);
			PlanningWeek week = weekNavigableMap.get(weekDag);
			if (week == null)
			{
				week = new PlanningWeek(weekDag);
				weekNavigableMap.put(weekDag, week);
			}
			week.getDagList().add(dag);

			dag.setWeek(week);
		}

		herhalingsWeek = getWeek(herhalingsWeekMaandag.isBefore(PlanningConstanten.plannenVanafDatum) ? PlanningConstanten.plannenVanafDatum : herhalingsWeekMaandag);
		initieelHerhalingsWeek = herhalingsWeek;
	}

	public PlanningScreeningsEenheid(Long id, Date uitgenodigdTotEnMet, Date uitnodigenTotEnMet, BigDecimal initieelInterval, Date herhalingsWeekMaandag,
		Long aantalMammografen, Integer maxStandplaatsPeriodeVolgnummer)
	{
		this(id, DateUtil.toLocalDate(uitgenodigdTotEnMet), DateUtil.toLocalDate(uitnodigenTotEnMet), initieelInterval, DateUtil.toLocalDate(herhalingsWeekMaandag));
		this.aantalMammografen = Math.max(1, aantalMammografen.intValue());  
		this.volgNrOffset = maxStandplaatsPeriodeVolgnummer == null ? 0 : maxStandplaatsPeriodeVolgnummer + 1;
	}

	public PlanningScreeningsOrganisatie getScreeningsOrganisatie()
	{
		return screeningsOrganisatie;
	}

	public void setScreeningsOrganisatie(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		this.screeningsOrganisatie = screeningsOrganisatie;
	}

	public NavigableSet<PlanningStandplaatsPeriode> getStandplaatsPeriodeNavigableSet()
	{
		return standplaatsPeriodeNavigableSet;
	}

	public Set<PlanningBlok> getBlokSet()
	{
		return blokSet;
	}

	public NavigableMap<LocalDate, PlanningDag> getDagNavigableMap()
	{
		return dagNavigableMap;
	}

	public NavigableMap<LocalDate, PlanningWeek> getWeekNavigableMap()
	{
		return weekNavigableMap;
	}

	public LocalDate getUitgenodigdTotEnMet()
	{
		return uitgenodigdTotEnMet;
	}

	public LocalDate getUitnodigenTotEnMet()
	{
		return uitnodigenTotEnMet;
	}

	public Integer getAantalMammografen()
	{
		return aantalMammografen;
	}

	public void setAantalMammografen(Integer aantalMammografen)
	{
		this.aantalMammografen = aantalMammografen;
	}

	public MammaMeldingNiveau getNiveau()
	{
		return niveau;
	}

	public void setNiveau(MammaMeldingNiveau niveau)
	{
		this.niveau = niveau;
	}

	public BigDecimal getInterval()
	{
		return interval;
	}

	public void setInterval(BigDecimal interval)
	{
		this.interval = interval;
	}

	public BigDecimal getInitieelInterval()
	{
		return initieelInterval;
	}

	public void setInitieelInterval(BigDecimal initieelInterval)
	{
		this.initieelInterval = initieelInterval;
	}

	public PlanningWeek getHerhalingsWeek()
	{
		return herhalingsWeek;
	}

	public void setHerhalingsWeek(PlanningWeek herhalingsWeek)
	{
		this.herhalingsWeek = herhalingsWeek;
	}

	public PlanningWeek getInitieelHerhalingsWeek()
	{
		return initieelHerhalingsWeek;
	}

	public void setInitieelHerhalingsWeek(PlanningWeek initieelHerhalingsWeek)
	{
		this.initieelHerhalingsWeek = initieelHerhalingsWeek;
	}

	public PlanningWeek getWeek(LocalDate maandag)
	{
		return weekNavigableMap.get(maandag);
	}

	public void putDag(LocalDate date, PlanningDag dag)
	{
		dagNavigableMap.put(date, dag);
	}

	public PlanningDag getDag(LocalDate dag)
	{
		return dagNavigableMap.get(dag);
	}

	public int getVolgNrOffset()
	{
		return volgNrOffset;
	}

	public void setVolgNrOffset(int volgNrOffset)
	{
		this.volgNrOffset = volgNrOffset;
	}
}
