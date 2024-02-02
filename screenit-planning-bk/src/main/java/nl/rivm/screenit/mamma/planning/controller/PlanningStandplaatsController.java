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

import java.util.List;
import java.util.NavigableSet;

import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsDto;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStandplaatsIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.service.PlanningAfspraakDrempelOverzichtService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController()
@RequestMapping("/" + PlanningRestConstants.C_STANDPLAATS)
public class PlanningStandplaatsController
{

	private final PlanningAfspraakDrempelOverzichtService afspraakDrempelOverzichtService;

	public PlanningStandplaatsController(PlanningAfspraakDrempelOverzichtService afspraakDrempelOverzichtService)
	{
		this.afspraakDrempelOverzichtService = afspraakDrempelOverzichtService;
	}

	@RequestMapping(method = RequestMethod.POST)
	public void post(@RequestBody PlanningStandplaatsDto standplaatsDto)
	{
		addOrChangeStandplaats(standplaatsDto);
	}

	@RequestMapping(method = RequestMethod.PUT)
	public void put(@RequestBody PlanningStandplaatsDto standplaatsDto)
	{
		addOrChangeStandplaats(standplaatsDto);
	}

	@RequestMapping(value = "/zonderRoute/{screeningsOrganisatieId}", method = RequestMethod.GET)
	public ResponseEntity<Long[]> getZonderRoute(@PathVariable Long screeningsOrganisatieId)
	{
		List<Long> standplaatsenZonderRonde = PlanningStandplaatsIndex.getStandplaatsenZonderRoute(screeningsOrganisatieId);
		ResponseEntity<Long[]> response = new ResponseEntity<>(standplaatsenZonderRonde.toArray(new Long[] {}), HttpStatus.OK);
		return response;
	}

	@RequestMapping(value = "/metRoute/{screeningsOrganisatieId}", method = RequestMethod.GET)
	public ResponseEntity<Long[]> getMetRoute(@PathVariable Long screeningsOrganisatieId)
	{
		List<Long> standplaatsenMetRonde = PlanningStandplaatsIndex.getStandplaatsenMetRoute(screeningsOrganisatieId);
		ResponseEntity<Long[]> response = new ResponseEntity<>(standplaatsenMetRonde.toArray(new Long[] {}), HttpStatus.OK);
		return response;
	}

	@RequestMapping(value = "/{standplaatsId}", method = RequestMethod.DELETE)
	public void delete(@PathVariable Long standplaatsId)
	{
		PlanningStandplaats knownStandplaats = PlanningStandplaatsIndex.get(standplaatsId);
		if (knownStandplaats != null)
		{
			knownStandplaats.getScreeningsOrganisatie().getStandplaatsSet().remove(knownStandplaats);

			for (PlanningStandplaatsRonde standplaatsRonde : knownStandplaats.getStandplaatsRondeNavigableSet())
			{
				for (PlanningStandplaatsPeriode standplaatsPeriode : standplaatsRonde.getStandplaatsPeriodeNavigableSet())
				{
					PlanningScreeningsEenheid screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
					NavigableSet<PlanningStandplaatsPeriode> standplaatsPeriodeNavigableSet = screeningsEenheid.getStandplaatsPeriodeNavigableSet();
					PlanningStandplaatsPeriode volgendeStandplaatsPeriode = standplaatsPeriodeNavigableSet.higher(standplaatsPeriode);
					standplaatsPeriodeNavigableSet.remove(standplaatsPeriode);
					if (volgendeStandplaatsPeriode != null)
					{
						PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid).setVanafStandplaatsPeriode(
							PlanningRouteController.decrementIndex(standplaatsPeriodeNavigableSet.size() + 1, standplaatsPeriode.getScreeningsEenheidVolgNr(), screeningsEenheid));
					}
					else
					{
						PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid)
							.setVanafStandplaatsPeriode(!standplaatsPeriodeNavigableSet.isEmpty() ? standplaatsPeriodeNavigableSet.last() : null);
					}
				}
			}
			PlanningStandplaatsIndex.remove(knownStandplaats);
			PlanningDoorrekenenManager.run();
		}
	}

	private void addOrChangeStandplaats(PlanningStandplaatsDto standplaatsDto)
	{
		PlanningScreeningsOrganisatie screeningsOrganisatie = PlanningScreeningsOrganisatieIndex.get(standplaatsDto.screeningsOrganisatieId);
		PlanningStandplaats knownStandplaats = PlanningStandplaatsIndex.get(standplaatsDto.id);
		if (knownStandplaats == null)
		{
			knownStandplaats = new PlanningStandplaats(standplaatsDto.id);
			knownStandplaats.setScreeningsOrganisatie(screeningsOrganisatie);
			PlanningStandplaatsIndex.put(knownStandplaats);
		}
		if (!knownStandplaats.getScreeningsOrganisatie().equals(screeningsOrganisatie))
		{
			knownStandplaats.getScreeningsOrganisatie().getStandplaatsSet().remove(knownStandplaats);
			knownStandplaats.setScreeningsOrganisatie(screeningsOrganisatie);
		}
		screeningsOrganisatie.getStandplaatsSet().add(knownStandplaats);

		PlanningWijzigingen.getStandplaatsSet().add(knownStandplaats);
	}

	@RequestMapping(value = "/getAfspraakDrempelOverzicht/{standplaatsId}", method = RequestMethod.GET)
	public ResponseEntity<PlanningStandplaatsDto> getAfspraakDrempelOverzicht(@PathVariable long standplaatsId)
	{
		PlanningStandplaats standplaats = PlanningStandplaatsIndex.get(standplaatsId);
		return new ResponseEntity(afspraakDrempelOverzichtService.getAfspraakDrempelOverzicht(standplaats), HttpStatus.OK);
	}
}
