package nl.rivm.screenit.mamma.planning.controller;

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

import java.util.NavigableSet;
import java.util.Set;

import nl.rivm.screenit.dto.mamma.planning.PlanningAfspraakDrempelOverzichtDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsOrganisatieDto;
import nl.rivm.screenit.mamma.planning.index.PlanningClientFactorTypeIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeks;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;
import nl.rivm.screenit.mamma.planning.service.PlanningAfspraakDrempelOverzichtService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningWijzigingen;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController()
@RequestMapping("/" + PlanningRestConstants.C_SCREENINGS_ORGANISATIE)
public class PlanningScreeningsOrganisatieController
{
	@Autowired
	private PlanningAfspraakDrempelOverzichtService afspraakDrempelOverzichtService;

	@RequestMapping(method = RequestMethod.PUT)
	public void put(@RequestBody PlanningScreeningsOrganisatieDto screeningsOrganisatieDto)
	{
		PlanningScreeningsOrganisatie screeningsOrganisatie = PlanningScreeningsOrganisatieIndex.get(screeningsOrganisatieDto.id);
		if (!screeningsOrganisatie.getFactorEersteOnderzoek().equals(screeningsOrganisatieDto.factorEersteOnderzoekBk))
		{
			screeningsOrganisatie.setFactorEersteOnderzoek(screeningsOrganisatieDto.factorEersteOnderzoekBk);
			bepaalWijzigingen(MammaFactorType.EERSTE_ONDERZOEK, screeningsOrganisatie);
		}
		if (!screeningsOrganisatie.getFactorDubbeleTijd().equals(screeningsOrganisatieDto.factorDubbeleTijdBk))
		{
			screeningsOrganisatie.setFactorDubbeleTijd(screeningsOrganisatieDto.factorDubbeleTijdBk);
			bepaalWijzigingen(MammaFactorType.DUBBELE_TIJD, screeningsOrganisatie);
		}
		if (!screeningsOrganisatie.getFactorMinderValide().equals(screeningsOrganisatieDto.factorMinderValideBk))
		{
			screeningsOrganisatie.setFactorMinderValide(screeningsOrganisatieDto.factorMinderValideBk);
			bepaalWijzigingen(MammaFactorType.MINDER_VALIDE, screeningsOrganisatie);
		}

		if (screeningsOrganisatie.getVervallenCapaciteitsreserveringDagen() != screeningsOrganisatieDto.vervallenCapaciteitsreserveringDagenBk)
		{
			screeningsOrganisatie.setVervallenCapaciteitsreserveringDagen(screeningsOrganisatieDto.vervallenCapaciteitsreserveringDagenBk);
		}

		if (screeningsOrganisatie.getWekenVanTevorenUitnodigen() != screeningsOrganisatieDto.wekenVanTevorenUitnodigen)
		{
			screeningsOrganisatie.setWekenVanTevorenUitnodigen(screeningsOrganisatieDto.wekenVanTevorenUitnodigen);
		}

		bepaalWijzigingen(screeningsOrganisatie);

		PlanningDoorrekenenManager.run();
	}

	private void bepaalWijzigingen(MammaFactorType factorType, PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		PlanningWijzigingen.getClientSet().addAll(PlanningClientFactorTypeIndex.get(screeningsOrganisatie, factorType));

		Set<PlanningStandplaats> standplaatsSet = screeningsOrganisatie.getStandplaatsSet();
		PlanningWijzigingen.getStandplaatsSet().addAll(standplaatsSet);
		for (PlanningStandplaats standplaats : standplaatsSet)
		{
			Set<PlanningPostcodeReeks> postcodeReeksSet = standplaats.getPostcodeReeksSet();
			PlanningWijzigingen.getPostcodeReeksSet().addAll(postcodeReeksSet);
			for (PlanningPostcodeReeks planningPostcodeReeks : postcodeReeksSet)
			{
				PlanningWijzigingen.getPostcodeReeksRegioSet().addAll(planningPostcodeReeks.getPostcodeReeksRegios());
			}
		}

		if (factorType == MammaFactorType.DUBBELE_TIJD || factorType == MammaFactorType.MINDER_VALIDE)
		{
			PlanningWijzigingen.getTehuisSet().addAll(screeningsOrganisatie.getTehuisSet());
		}
	}

	private void bepaalWijzigingen(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		screeningsOrganisatie.getScreeningsEenheidSet().forEach(screeningsEenheid -> {
			NavigableSet<PlanningStandplaatsPeriode> standplaatsPeriodeNavigableSet = screeningsEenheid.getStandplaatsPeriodeNavigableSet();
			if (!standplaatsPeriodeNavigableSet.isEmpty())
			{
				PlanningWijzigingen.getWijzigingenRoute(screeningsEenheid).setVanafStandplaatsPeriode(standplaatsPeriodeNavigableSet.first());
			}
		});
	}

	@RequestMapping(value = "/getAfspraakDrempelOverzicht/{screeningsOrganisatieId}", method = RequestMethod.GET)
	public ResponseEntity<PlanningAfspraakDrempelOverzichtDto> getAfspraakDrempelOverzicht(@PathVariable long screeningsOrganisatieId)
	{
		PlanningScreeningsOrganisatie screeningsOrganisatie = PlanningScreeningsOrganisatieIndex.get(screeningsOrganisatieId);

		return new ResponseEntity(afspraakDrempelOverzichtService.getAfspraakDrempelOverzicht(screeningsOrganisatie), HttpStatus.OK);
	}
}
