package nl.rivm.screenit.mamma.planning.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto.PlanningMeldingDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto.PlanningMeldingenPerSeDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.exceptions.DryRunException;
import nl.rivm.screenit.exceptions.OpslaanAfsprakenBuitenStandplaatsPeriodeException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.exceptions.SeTijdBlokOverlapException;
import nl.rivm.screenit.mamma.planning.dao.PlanningReadModelDao;
import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsOrganisatieIndex;
import nl.rivm.screenit.mamma.planning.service.PlanningConceptOpslaanService;
import nl.rivm.screenit.mamma.planning.wijzigingen.PlanningDoorrekenenManager;
import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.Date;
import java.util.Map;

@RestController()
@RequestMapping("/" + PlanningRestConstants.C_ACTIE)
public class PlanningActieController
{
	private static final Logger LOG = LoggerFactory.getLogger(PlanningActieController.class);

	@Autowired
	PlanningReadModelDao readModelDao;

	@Autowired
	private PlanningConceptOpslaanService conceptOpslaanService;

	@Transactional(propagation = Propagation.REQUIRED)
	@RequestMapping(value = "/readModel", method = RequestMethod.POST)
	public void readModel()
	{
		readModelDao.readDataModel();

		PlanningDoorrekenenManager.run();
	}

	@RequestMapping(value = "/conceptOpslaan/{screeningOrganisatieId}/{runDry}", method = RequestMethod.GET)
	@ResponseBody
	public PlanningConceptMeldingenDto conceptOpslaan(@PathVariable Long screeningOrganisatieId, @PathVariable Boolean runDry)
	{
		try
		{
			return conceptOpslaanService.opslaan(screeningOrganisatieId, runDry);
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
			return meldingenDto;
		}
		catch (DryRunException e)
		{
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
			return meldingenDto;
		}
	}

	@Transactional(propagation = Propagation.REQUIRED)
	@RequestMapping(value = "/conceptAnnuleren/{screeningOrganisatieId}", method = RequestMethod.POST)
	public void conceptAnnueleren(@PathVariable Long screeningOrganisatieId)
	{
		readModelDao.reset(PlanningScreeningsOrganisatieIndex.get(screeningOrganisatieId));
	}

	@RequestMapping(value = "/conceptGewijzigdDoor/{instellingGebruikerId}/{screeningOrganisatieId}", method = RequestMethod.POST)
	public void conceptGewijzigdDoor(@PathVariable Long instellingGebruikerId, @PathVariable Long screeningOrganisatieId)
	{
		PlanningScreeningsOrganisatieIndex.get(screeningOrganisatieId).conceptGewijzigdDoor(instellingGebruikerId);
	}

	@RequestMapping(value = "/conceptGewijzigdDoor/{screeningOrganisatieId}", method = RequestMethod.GET)
	public ResponseEntity<Long[]> getConceptGewijzigdDoor(@PathVariable Long screeningOrganisatieId)
	{
		return new ResponseEntity<>(PlanningScreeningsOrganisatieIndex.get(screeningOrganisatieId).getConceptGewijzigdDoor().toArray(new Long[] {}), HttpStatus.OK);
	}
}
