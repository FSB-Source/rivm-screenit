package nl.rivm.screenit.mamma.planning.controller;

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

import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsPeriodeIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;

import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/" + PlanningRestConstants.C_STANDPLAATS_PERIODE)
public class PlanningStandplaatsPeriodeController
{
	@PutMapping
	public void put(@RequestBody PlanningStandplaatsPeriodeDto standplaatsPeriodeDto)
	{
		PlanningStandplaatsPeriode standplaatsPeriode = PlanningStandplaatsPeriodeIndex.get(standplaatsPeriodeDto.conceptId);
		PlanningStandplaatsRonde standplaatsRonde = standplaatsPeriode.getStandplaatsRonde();
		standplaatsRonde.setAfspraakDrempel(standplaatsPeriodeDto.afspraakDrempel);

		if (standplaatsPeriode.getPrognose() != standplaatsPeriodeDto.prognose ||
			!standplaatsPeriode.getVanaf().equals(standplaatsPeriodeDto.vanaf) ||
			!standplaatsPeriode.getTotEnMet().equals(standplaatsPeriodeDto.totEnMet))
		{
			standplaatsPeriode.setPrognose(standplaatsPeriodeDto.prognose);
			standplaatsPeriode.setVanaf(standplaatsPeriodeDto.vanaf);
			standplaatsPeriode.setTotEnMet(standplaatsPeriodeDto.totEnMet);

			PlanningWijzigingen.getWijzigingenRoute(standplaatsPeriode.getScreeningsEenheid()).setVanafStandplaatsPeriode(standplaatsPeriode);
			PlanningDoorrekenenManager.run();
		}

		standplaatsPeriode.getStandplaatsRonde().setAchtervangStandplaats(PlanningStandplaatsIndex.get(standplaatsPeriodeDto.achtervangStandplaatsId));
		standplaatsPeriode.getStandplaatsRonde().setMinderValideUitwijkStandplaats(PlanningStandplaatsIndex.get(standplaatsPeriodeDto.minderValideUitwijkStandplaatsId));
		standplaatsPeriode.getStandplaatsRonde().setMinderValideUitnodigenVanaf(standplaatsPeriodeDto.minderValideUitnodigenVanaf);
		standplaatsPeriode.getStandplaatsRonde().setAfspraakcapaciteitBeschikbaarVoor(
			standplaatsPeriodeDto.afspraakcapaciteitBeschikbaarVoorIds.stream().map(PlanningScreeningsOrganisatieIndex::get).toList());
	}
}
