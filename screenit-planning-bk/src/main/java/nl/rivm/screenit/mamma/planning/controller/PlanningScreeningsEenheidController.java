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

import java.time.DayOfWeek;

import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsEenheidDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsEenheidMetaDataDto;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/" + PlanningRestConstants.C_SCREENINGSEENHEID)
public class PlanningScreeningsEenheidController
{
	private final ICurrentDateSupplier dateSupplier;

	public PlanningScreeningsEenheidController(ICurrentDateSupplier dateSupplier)
	{
		this.dateSupplier = dateSupplier;
	}

	@PostMapping
	public void post(@RequestBody PlanningScreeningsEenheidDto screeningsEenheidDto)
	{
		addOrChangeScreeningsEenheid(screeningsEenheidDto);
	}

	@PutMapping
	public void put(@RequestBody PlanningScreeningsEenheidDto screeningsEenheidDto)
	{
		addOrChangeScreeningsEenheid(screeningsEenheidDto);
	}

	@DeleteMapping("/{screeningsEenheidId}")
	public void delete(@PathVariable Long screeningsEenheidId)
	{
		PlanningScreeningsEenheid knownScreeningsEenheid = PlanningScreeningsEenheidIndex.get(screeningsEenheidId);
		if (knownScreeningsEenheid != null)
		{
			knownScreeningsEenheid.getScreeningsOrganisatie().getScreeningsEenheidSet().remove(knownScreeningsEenheid);

			PlanningScreeningsEenheidIndex.remove(knownScreeningsEenheid);
		}
	}

	private void addOrChangeScreeningsEenheid(PlanningScreeningsEenheidDto screeningsEenheidDto)
	{
		PlanningScreeningsOrganisatie screeningsOrganisatie = PlanningScreeningsOrganisatieIndex.get(screeningsEenheidDto.screeningsOrganisatieId);
		PlanningScreeningsEenheid knownScreeningsEenheid = PlanningScreeningsEenheidIndex.get(screeningsEenheidDto.id);
		if (knownScreeningsEenheid == null)
		{
			knownScreeningsEenheid = new PlanningScreeningsEenheid(screeningsEenheidDto.id, null, null, null, dateSupplier.getLocalDate().with(DayOfWeek.MONDAY));
			knownScreeningsEenheid.setScreeningsOrganisatie(screeningsOrganisatie);
			PlanningScreeningsEenheidIndex.put(knownScreeningsEenheid);
		}
		knownScreeningsEenheid.setAantalMammografen(screeningsEenheidDto.aantalMammografen);
		if (!knownScreeningsEenheid.getScreeningsOrganisatie().equals(screeningsOrganisatie))
		{
			knownScreeningsEenheid.getScreeningsOrganisatie().getScreeningsEenheidSet().remove(knownScreeningsEenheid);
			knownScreeningsEenheid.setScreeningsOrganisatie(screeningsOrganisatie);
		}
		screeningsOrganisatie.getScreeningsEenheidSet().add(knownScreeningsEenheid);
	}

	@GetMapping("/metaData/{screeningsEenheidId}")
	public PlanningScreeningsEenheidMetaDataDto getMetaData(@PathVariable Long screeningsEenheidId)
	{
		return PlanningMapper.from(PlanningScreeningsEenheidIndex.get(screeningsEenheidId));
	}

}
