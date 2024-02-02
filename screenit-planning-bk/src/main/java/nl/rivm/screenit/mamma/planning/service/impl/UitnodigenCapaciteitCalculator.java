package nl.rivm.screenit.mamma.planning.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.planning.model.PlanningBeschikbaar;
import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.model.PopulatieMetStreefDatum;

@Slf4j
public class UitnodigenCapaciteitCalculator
{

	private final Set<PlanningDag> uitTeNodigenDagen;

	private final PlanningStandplaatsRonde standplaatsRonde;

	private final Set<PopulatieMetStreefDatum> standplaatsPopulatie;

	private final BigDecimal extraMindervalideCapaciteitUitgenodigd;

	private final PlanningScreeningsOrganisatie screeningsOrganisatieStandplaats;

	private BigDecimal capaciteitVoorUitnodigen;

	public UitnodigenCapaciteitCalculator(Set<PlanningDag> uitTeNodigenDagen, PlanningStandplaatsRonde standplaatsRonde,
		Set<PopulatieMetStreefDatum> standplaatsPopulatie, BigDecimal extraMindervalideCapaciteitUitgenodigd)
	{
		this.uitTeNodigenDagen = uitTeNodigenDagen;
		this.standplaatsRonde = standplaatsRonde;
		this.standplaatsPopulatie = standplaatsPopulatie;
		this.extraMindervalideCapaciteitUitgenodigd = extraMindervalideCapaciteitUitgenodigd;
		this.screeningsOrganisatieStandplaats = standplaatsRonde.getStandplaats().getScreeningsOrganisatie();
	}

	public BigDecimal berekenCapaciteitVoorUitnodigen()
	{
		initialiseerBeschikbareCapaciteit();
		reserveerWijkSelectieEnUitstel();
		reserveerGeplandeAfspraken();
		reserveerUitverhuizers();
		corrigeerExtraMinderValideUitnodigingen();
		return capaciteitVoorUitnodigen;
	}

	private void initialiseerBeschikbareCapaciteit()
	{
		PlanningBeschikbaar beschikbaar = new PlanningBeschikbaar();
		for (PlanningDag dag : uitTeNodigenDagen)
		{
			if (dag.getDatum().isBefore(PlanningConstanten.prognoseVanafDatum))
			{

				beschikbaar.add(dag.getBeschikbaar());
			}
			else if (dag.getBlokkadeSet().isEmpty())
			{
				beschikbaar.add(dag.getBeschikbaar());
			}
		}

		capaciteitVoorUitnodigen = beschikbaar.getTotaalRegulier(); 
		LOG.info("{}: initieel beschikbaar: {}", logPrefix(), capaciteitVoorUitnodigen);
	}

	private void reserveerWijkSelectieEnUitstel()
	{
		AtomicInteger aantalUitstel = new AtomicInteger();
		standplaatsPopulatie.forEach(wijkPopulatie -> wijkPopulatie.getClienten().forEach(client ->
		{
			if (wijkPopulatie.isVoorUitstelClient())
			{
				reserveerUitstel(client);
				aantalUitstel.incrementAndGet();
			}
			else
			{
				reserveerWijkSelectie(client);
			}
		}));
		LOG.info("{}: na reserveringen wijkselectie en uitstel: {}. Aantal uitstel: {}", logPrefix(), capaciteitVoorUitnodigen, aantalUitstel.get());
	}

	private void reserveerWijkSelectie(PlanningClient client)
	{
		if (client.getAfspraakStandplaats() == null && client.isUitgenodigdHuidigeStandplaatsRonde())
		{

			reserveerIndienReserveringZonderAfspraakNogGeldig(client);
		}
	}

	private void reserveerIndienReserveringZonderAfspraakNogGeldig(PlanningClient client)
	{
		if (openUitnodigingReserveringNogGeldig(client))
		{
			reserveerCapaciteitVoor(client);
		}
	}

	private boolean openUitnodigingReserveringNogGeldig(PlanningClient client)
	{
		if (client.getLaatsteUitnodigingDatum() != null)
		{
			LocalDate reserveringVerlooptOp = client.getLaatsteUitnodigingDatum().plusDays(screeningsOrganisatieStandplaats.getVervallenCapaciteitsreserveringDagen());
			return !PlanningConstanten.prognoseVanafDatum.isAfter(reserveringVerlooptOp);
		}
		return false;
	}

	private void reserveerCapaciteitVoor(PlanningClient client)
	{
		if (!client.inTehuis())
		{
			capaciteitVoorUitnodigen = capaciteitVoorUitnodigen.subtract(client.getBenodigdeCapaciteit(screeningsOrganisatieStandplaats));
		}
	}

	private void reserveerUitstel(PlanningClient client)
	{
		if (client.getUitgenodigdNaUitstel())
		{

			reserveerIndienReserveringZonderAfspraakNogGeldig(client);
		}
	}

	private void reserveerGeplandeAfspraken()
	{
		PlanningStandplaats standplaats = standplaatsRonde.getStandplaats();
		Set<PlanningClient> afspraken = standplaats.getAfspraakSet(); 
		afspraken.forEach(client ->
		{
			if (!client.isNoShow())
			{
				reserveerCapaciteitVoor(client);
			}
		});
		LOG.info("{}: na reserveringen geplande afspraken: {}. Aantal afspraken: {}", logPrefix(), capaciteitVoorUitnodigen, afspraken.size());
	}

	private void reserveerUitverhuizers()
	{

		Set<PlanningClient> uitverhuizers = standplaatsRonde.getScreeningRondeTransportSet();
		uitverhuizers.forEach(this::reserveerIndienReserveringZonderAfspraakNogGeldig);
		LOG.info("{}: na reserveringen uitverhuizers: {}. Aantal uitverhuizers: {}", logPrefix(), capaciteitVoorUitnodigen, uitverhuizers.size());
	}

	private void corrigeerExtraMinderValideUitnodigingen()
	{
		capaciteitVoorUitnodigen = capaciteitVoorUitnodigen.add(extraMindervalideCapaciteitUitgenodigd);
		LOG.info("{}: na correctie extra mindervalide uitnodigingen: {}. extraMindervalideCapaciteitUitgenodigd: {}", logPrefix(), capaciteitVoorUitnodigen,
			extraMindervalideCapaciteitUitgenodigd);
	}

	private String logPrefix()
	{
		return "capaciteitVoorUitnodigen SPR: " + standplaatsRonde.getId();
	}
}
