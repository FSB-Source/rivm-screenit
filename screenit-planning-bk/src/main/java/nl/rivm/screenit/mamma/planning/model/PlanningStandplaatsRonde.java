package nl.rivm.screenit.mamma.planning.model;

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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;

import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;

public class PlanningStandplaatsRonde extends PlanningConceptEntiteit
{
	private PlanningStandplaats standplaats;

	private LocalDate gewogenGemiddeldeDatum;

	private BigDecimal beschikbaarTotaal;

	private Integer afspraakDrempel;

	private static final Comparator<PlanningStandplaatsPeriode> standplaatsPeriodeComparator = Comparator.comparing(PlanningStandplaatsPeriode::getStandplaatsRondeVolgNr);

	private final NavigableSet<PlanningStandplaatsPeriode> standplaatsPeriodeNavigableSet = new TreeSet<>(standplaatsPeriodeComparator);

	private MammaMeldingNiveau niveau = MammaMeldingNiveau.INFO;

	private final List<PlanningMelding> meldingList = new ArrayList<>();

	private BigDecimal interval;

	private BigDecimal intieelInterval;

	private PlanningStandplaats achtervangStandplaats;

	private PlanningStandplaats minderValideUitwijkStandplaats;

	private List<PlanningScreeningsOrganisatie> afspraakcapaciteitBeschikbaarVoor = new ArrayList<>();

	private Boolean achtervangToegepast;

	private LocalDate minderValideUitnodigenVanaf;

	private BigDecimal extraMinderValideCapaciteitUitgenodigd;

	private final Set<PlanningClient> screeningRondeTransportSet = new HashSet<>();

	public PlanningStandplaatsRonde(Long id, Integer afspraakDrempel, BigDecimal intieelInterval, PlanningStandplaats achtervangStandplaats,
		PlanningStandplaats minderValideUitwijkStandplaats, Boolean achtervangToegepast, LocalDate minderValideUitnodigenVanaf, BigDecimal extraMinderValideCapaciteitUitgenodigd)
	{
		super(id);
		this.afspraakDrempel = afspraakDrempel;
		this.intieelInterval = intieelInterval;
		this.achtervangStandplaats = achtervangStandplaats;
		this.minderValideUitwijkStandplaats = minderValideUitwijkStandplaats;
		this.achtervangToegepast = achtervangToegepast;
		this.minderValideUitnodigenVanaf = minderValideUitnodigenVanaf;
		this.extraMinderValideCapaciteitUitgenodigd = extraMinderValideCapaciteitUitgenodigd;
	}

	public PlanningStandplaats getStandplaats()
	{
		return standplaats;
	}

	public void setStandplaats(PlanningStandplaats standplaats)
	{
		this.standplaats = standplaats;
	}

	public NavigableSet<PlanningStandplaatsPeriode> getStandplaatsPeriodeNavigableSet()
	{
		return standplaatsPeriodeNavigableSet;
	}

	public MammaMeldingNiveau getNiveau()
	{
		return niveau;
	}

	public void setNiveau(MammaMeldingNiveau niveau)
	{
		this.niveau = niveau;
	}

	public List<PlanningMelding> getMeldingList()
	{
		return meldingList;
	}

	public Integer getAfspraakDrempel()
	{
		return afspraakDrempel;
	}

	public void setAfspraakDrempel(Integer afspraakDrempel)
	{
		this.afspraakDrempel = afspraakDrempel;
	}

	public LocalDate getGewogenGemiddeldeDatum()
	{
		return gewogenGemiddeldeDatum;
	}

	public void setGewogenGemiddeldeDatum(LocalDate gewogenGemiddeldeDatum)
	{
		this.gewogenGemiddeldeDatum = gewogenGemiddeldeDatum;
	}

	public BigDecimal getBeschikbaarTotaal()
	{
		return beschikbaarTotaal;
	}

	public void setBeschikbaarTotaal(BigDecimal beschikbaarTotaal)
	{
		this.beschikbaarTotaal = beschikbaarTotaal;
	}

	public void clear()
	{
		gewogenGemiddeldeDatum = null;
		beschikbaarTotaal = BigDecimal.ZERO;
	}

	public BigDecimal getInterval()
	{
		return interval;
	}

	public void setInterval(BigDecimal interval)
	{
		this.interval = interval;
	}

	public BigDecimal getIntieelInterval()
	{
		return intieelInterval;
	}

	public void setIntieelInterval(BigDecimal intieelInterval)
	{
		this.intieelInterval = intieelInterval;
	}

	public PlanningStandplaats getAchtervangStandplaats()
	{
		return achtervangStandplaats;
	}

	public void setAchtervangStandplaats(PlanningStandplaats achtervangStandplaats)
	{
		this.achtervangStandplaats = achtervangStandplaats;
	}

	public PlanningStandplaats getMinderValideUitwijkStandplaats()
	{
		return minderValideUitwijkStandplaats;
	}

	public void setMinderValideUitwijkStandplaats(PlanningStandplaats minderValideUitwijkStandplaats)
	{
		this.minderValideUitwijkStandplaats = minderValideUitwijkStandplaats;
	}

	public Boolean getAchtervangToegepast()
	{
		return achtervangToegepast;
	}

	public void setAchtervangToegepast(Boolean achtervangToegepast)
	{
		this.achtervangToegepast = achtervangToegepast;
	}

	public Set<PlanningClient> getScreeningRondeTransportSet()
	{
		return screeningRondeTransportSet;
	}

	public LocalDate getMinderValideUitnodigenVanaf()
	{
		return minderValideUitnodigenVanaf;
	}

	public void setMinderValideUitnodigenVanaf(LocalDate minderValideUitnodigenVanaf)
	{
		this.minderValideUitnodigenVanaf = minderValideUitnodigenVanaf;
	}

	public BigDecimal getExtraMinderValideCapaciteitUitgenodigd()
	{
		return extraMinderValideCapaciteitUitgenodigd;
	}

	public void setExtraMinderValideCapaciteitUitgenodigd(BigDecimal extraMinderValideCapaciteitUitgenodigd)
	{
		this.extraMinderValideCapaciteitUitgenodigd = extraMinderValideCapaciteitUitgenodigd;
	}

	public List<PlanningScreeningsOrganisatie> getAfspraakcapaciteitBeschikbaarVoor()
	{
		return afspraakcapaciteitBeschikbaarVoor;
	}

	public void setAfspraakcapaciteitBeschikbaarVoor(List<PlanningScreeningsOrganisatie> afspraakcapaciteitBeschikbaarVoor)
	{
		this.afspraakcapaciteitBeschikbaarVoor = afspraakcapaciteitBeschikbaarVoor;
	}

	public static Comparator<PlanningStandplaatsPeriode> getStandplaatsPeriodeComparator()
	{
		return standplaatsPeriodeComparator;
	}
}
