package nl.rivm.screenit.mamma.planning.controller;

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

import java.util.Date;
import java.util.Map;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto.PlanningMeldingDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto.PlanningMeldingenPerSeDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.exceptions.DryRunException;
import nl.rivm.screenit.exceptions.OpslaanAfsprakenBuitenStandplaatsPeriodeException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.exceptions.SeTijdBlokOverlapException;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;
import nl.rivm.screenit.mamma.planning.index.PlanningStatusIndex;
import nl.rivm.screenit.mamma.planning.service.PlanningConceptOpslaanService;
import nl.rivm.screenit.mamma.planning.service.PlanningConceptmodelService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;
import nl.rivm.screenit.model.mamma.enums.MammaPlanningStatus;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/" + PlanningRestConstants.C_ACTIE)
@AllArgsConstructor
public class PlanningActieController
{
	private final PlanningConceptmodelService conceptModelService;

	private final PlanningConceptOpslaanService conceptOpslaanService;

	@Transactional(propagation = Propagation.REQUIRED)
	@PostMapping(value = "/" + PlanningRestConstants.C_READMODEL)
	public void readModel()
	{
		PlanningStatusIndex.set(MammaPlanningStatus.RESET_MODEL);
		try
		{
			conceptModelService.resetConceptmodel();
			PlanningDoorrekenenManager.run();
			PlanningStatusIndex.set(MammaPlanningStatus.OPERATIONEEL);
		}
		catch (Exception e)
		{
			PlanningStatusIndex.set(MammaPlanningStatus.ERROR);
			throw e;
		}
	}

	@GetMapping(value = "/conceptOpslaan/{screeningOrganisatieId}/{runDry}")
	@ResponseBody
	public PlanningConceptMeldingenDto conceptOpslaan(@PathVariable Long screeningOrganisatieId, @PathVariable boolean runDry)
	{
		try
		{
			if (runDry)
			{
				PlanningStatusIndex.set(MammaPlanningStatus.CONCEPT_WIJZIGINGEN_BEPALEN, screeningOrganisatieId);
			}
			else
			{
				PlanningStatusIndex.set(MammaPlanningStatus.CONCEPT_WIJZIGINGEN_OPSLAAN, screeningOrganisatieId);
			}
			PlanningConceptMeldingenDto opslaanMeldingenDto = conceptOpslaanService.slaConceptOpVoorScreeningsOrganisatie(screeningOrganisatieId, runDry);
			PlanningStatusIndex.set(MammaPlanningStatus.OPERATIONEEL);
			return opslaanMeldingenDto;
		}
		catch (OpslaanVerwijderenTijdBlokException e)
		{
			PlanningConceptMeldingenDto meldingenDto = new PlanningConceptMeldingenDto();
			meldingenDto.niveau = MammaMeldingNiveau.PROBLEEM;

			PlanningMeldingDto meldingDto = new PlanningMeldingDto();
			meldingDto.niveau = MammaMeldingNiveau.PROBLEEM;
			meldingDto.tekst = e.getMessage() + e.getAdditionalMessageInfo();
			SeTijdBlokOverlapException seTijdBlokOverlapException = (SeTijdBlokOverlapException) e;
			PlanningMeldingenPerSeDto meldingenPerSeDto = new PlanningMeldingenPerSeDto();
			meldingenPerSeDto.niveau = MammaMeldingNiveau.PROBLEEM;
			meldingenPerSeDto.meldingen.add(meldingDto);
			meldingenDto.seMeldingen.put(seTijdBlokOverlapException.getScreeningsEenheidId(), meldingenPerSeDto);
			PlanningStatusIndex.set(MammaPlanningStatus.OPERATIONEEL);
			return meldingenDto;
		}
		catch (DryRunException e)
		{
			PlanningStatusIndex.set(MammaPlanningStatus.OPERATIONEEL);
			return e.getMeldingen();
		}
		catch (OpslaanAfsprakenBuitenStandplaatsPeriodeException e)
		{
			PlanningConceptMeldingenDto meldingenDto = new PlanningConceptMeldingenDto();
			meldingenDto.niveau = MammaMeldingNiveau.PROBLEEM;

			for (Map.Entry<Long, Date[]> entry : e.getAfsprakenBuitenStandplaatsPeriodeMap().entrySet())
			{
				PlanningMeldingDto meldingDto = new PlanningMeldingDto();
				meldingDto.niveau = MammaMeldingNiveau.PROBLEEM;
				meldingDto.tekst = e.getMessage(entry.getValue());

				PlanningMeldingenPerSeDto meldingenPerSeDto = new PlanningMeldingenPerSeDto();
				meldingenPerSeDto.niveau = MammaMeldingNiveau.PROBLEEM;
				meldingenPerSeDto.meldingen.add(meldingDto);
				meldingenDto.seMeldingen.put(entry.getKey(), meldingenPerSeDto);
			}
			PlanningStatusIndex.set(MammaPlanningStatus.OPERATIONEEL);
			return meldingenDto;
		}
		catch (Exception e)
		{
			PlanningStatusIndex.set(MammaPlanningStatus.ERROR);
			throw e;
		}
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@PostMapping(value = "/conceptAnnuleren/{screeningOrganisatieId}")
	public void conceptAnnueleren(@PathVariable Long screeningOrganisatieId)
	{
		try
		{
			PlanningStatusIndex.set(MammaPlanningStatus.CONCEPT_ANNULEREN, screeningOrganisatieId);
			conceptModelService.annuleerConceptmodelWijzigingen(PlanningScreeningsOrganisatieIndex.get(screeningOrganisatieId));
			conceptOpslaanService.slaConceptOpVoorAlleScreeningsOrganisaties();

			PlanningStatusIndex.set(MammaPlanningStatus.OPERATIONEEL);
		}
		catch (Exception e)
		{
			PlanningStatusIndex.set(MammaPlanningStatus.ERROR);
			throw e;
		}
	}

	@PostMapping(value = "/conceptGewijzigdDoor/{instellingGebruikerId}/{screeningOrganisatieId}")
	public void conceptGewijzigdDoor(@PathVariable Long instellingGebruikerId, @PathVariable Long screeningOrganisatieId)
	{
		PlanningScreeningsOrganisatieIndex.get(screeningOrganisatieId).conceptGewijzigdDoor(instellingGebruikerId);
	}

	@GetMapping(value = "/conceptGewijzigdDoor/{screeningOrganisatieId}")
	public ResponseEntity<Long[]> getConceptGewijzigdDoor(@PathVariable Long screeningOrganisatieId)
	{
		return new ResponseEntity<>(PlanningScreeningsOrganisatieIndex.get(screeningOrganisatieId).getConceptGewijzigdDoor().toArray(new Long[] {}), HttpStatus.OK);
	}
}
