package nl.rivm.screenit.mamma.planning.model;

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

import java.time.LocalDate;
import java.util.HashSet;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;

public final class PlanningStandplaats extends PlanningEntiteit
{
	private PlanningScreeningsOrganisatie screeningsOrganisatie;

	private final Set<PlanningPostcodeReeks> postcodeReeksSet = new HashSet<>();

	private LocalDate vorigeGewogenGemiddeldeDatum;

	private final NavigableSet<PlanningStandplaatsRonde> standplaatsRondeNavigableSet = new TreeSet<>(
		(standplaatsRonde1, standplaatsRonde2) ->
		{
			int compareTo = standplaatsRonde1.getStandplaatsPeriodeNavigableSet().first().getVanaf()
				.compareTo(standplaatsRonde2.getStandplaatsPeriodeNavigableSet().first().getVanaf());
			if (compareTo != 0)
			{
				return compareTo;
			}
			return standplaatsRonde1.getConceptId().compareTo(standplaatsRonde2.getConceptId());
		});

	private final PlanningBenodigd benodigd = new PlanningBenodigd();

	private final PlanningBenodigd transport = new PlanningBenodigd();

	private final Set<PlanningClient> transportVanSet = new HashSet<>();

	private final Set<PlanningClient> transportNaarSet = new HashSet<>();

	private final Set<PlanningClient> uitstelSet = new HashSet<>();

	private final Set<PlanningClient> afspraakSet = new HashSet<>();

	private final Set<PlanningTehuis> tehuisSet = new HashSet<>();

	public PlanningStandplaats(Long id)
	{
		super(id);
	}

	public PlanningScreeningsOrganisatie getScreeningsOrganisatie()
	{
		return screeningsOrganisatie;
	}

	public void setScreeningsOrganisatie(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		this.screeningsOrganisatie = screeningsOrganisatie;
	}

	public Set<PlanningPostcodeReeks> getPostcodeReeksSet()
	{
		return postcodeReeksSet;
	}

	public NavigableSet<PlanningStandplaatsRonde> getStandplaatsRondeNavigableSet()
	{
		return standplaatsRondeNavigableSet;
	}

	public PlanningBenodigd getBenodigd()
	{
		return benodigd;
	}

	public Set<PlanningClient> getUitstelSet()
	{
		return uitstelSet;
	}

	public PlanningBenodigd getTransport()
	{
		return transport;
	}

	public LocalDate getVorigeGewogenGemiddeldeDatum()
	{
		return vorigeGewogenGemiddeldeDatum;
	}

	public void setVorigeGewogenGemiddeldeDatum(LocalDate vorigeGewogenGemiddeldeDatum)
	{
		this.vorigeGewogenGemiddeldeDatum = vorigeGewogenGemiddeldeDatum;
	}

	public Set<PlanningTehuis> getTehuisSet()
	{
		return tehuisSet;
	}

	public Set<PlanningClient> getAfspraakSet()
	{
		return afspraakSet;
	}

	public Set<PlanningClient> getTransportVanSet()
	{
		return transportVanSet;
	}

	public Set<PlanningClient> getTransportNaarSet()
	{
		return transportNaarSet;
	}
}
