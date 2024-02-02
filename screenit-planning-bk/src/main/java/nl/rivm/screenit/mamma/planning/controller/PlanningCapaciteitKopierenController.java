package nl.rivm.screenit.mamma.planning.controller;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.dto.mamma.planning.PlanningDagKopierenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.service.PlanningCapaciteitAgendaService;
import nl.rivm.screenit.mamma.planning.service.PlanningWijzigingenBepalenService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;

import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/" + PlanningRestConstants.C_DAG_KOPIEREN)
@AllArgsConstructor
public class PlanningCapaciteitKopierenController
{
	private final PlanningCapaciteitAgendaService capaciteitAgendaService;

	private final PlanningWijzigingenBepalenService wijzigingenBepalenService;

	@PutMapping
	public void kopieerDag(@RequestBody PlanningDagKopierenDto kopierenDto) throws OpslaanVerwijderenTijdBlokException
	{
		var bronScreeningsEenheid = PlanningScreeningsEenheidIndex.get(kopierenDto.bronScreeningsEenheidId);
		var doelScreeningsEenheid = PlanningScreeningsEenheidIndex.get(kopierenDto.doelScreeningsEenheidId);

		var nieuweDoelDag = capaciteitAgendaService.kopieerDag(bronScreeningsEenheid, doelScreeningsEenheid, kopierenDto.bronDatum, kopierenDto.bronVanTijd,
			kopierenDto.bronTotTijd,
			kopierenDto.doelDatum);

		wijzigingenBepalenService.bepaalWijzigingen(nieuweDoelDag);
		PlanningDoorrekenenManager.run();
	}
}
