package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import javax.validation.Valid;

import nl.rivm.screenit.huisartsenportaal.dto.AanvraagDto;
import nl.rivm.screenit.huisartsenportaal.dto.AanvraagTotalenDto;
import nl.rivm.screenit.huisartsenportaal.dto.AanvragenZoekObjectDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.LabformulierAanvraag;
import nl.rivm.screenit.huisartsenportaal.service.LabformulierService;
import nl.rivm.screenit.huisartsenportaal.service.LocatieService;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;
import nl.rivm.screenit.huisartsenportaal.validator.LabformulierenAanvragenValidator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("aanvragen")
@PreAuthorize("isAuthenticated()")
public class AanvraagController extends BaseController
{
	@Autowired
	private LabformulierService labformulierService;

	@Autowired
	private LocatieService locatieService;

	@Autowired
	private SynchronisatieService synchronisatieService;

	@Autowired
	private LabformulierenAanvragenValidator maximaleLabformulierenAanvragenValidator;

	@InitBinder
	public void dataBinding(WebDataBinder binder)
	{
		binder.addValidators(maximaleLabformulierenAanvragenValidator);
	}

	@RequestMapping(value = "/huisarts", method = RequestMethod.POST)
	@PreAuthorize("hasRole('ROLE_AANVRAGEN')")
	public ResponseEntity<AanvraagTotalenDto> getHuidigeAanvragen(@RequestBody AanvragenZoekObjectDto zoekObjectDto, BindingResult result)
	{
		AanvraagTotalenDto aanvragen = labformulierService.getAanvragenHuisarts(getIngelogdeHuisarts(), zoekObjectDto.getResultOptions());
		return new ResponseEntity<>(aanvragen, HttpStatus.OK);
	}

	@RequestMapping(value = "/huisarts/{huisartsId}", method = RequestMethod.POST)
	@PreAuthorize("hasRole('ROLE_AANVRAGEN')")
	public ResponseEntity postAanvraag(@PathVariable String huisartsId, @Valid @RequestBody AanvraagDto aanvraagDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			return ResponseEntity.badRequest().body(result.getAllErrors());
		}

		Huisarts huisarts = getIngelogdeHuisarts();
		LabformulierAanvraag aanvraag = labformulierService.saveAanvraag(huisarts, aanvraagDto);
		synchronisatieService.syncAanvraag(aanvraag);

		return ResponseEntity.ok(null);
	}

	@RequestMapping(value = "/locatie/{locatieId}/stats", method = RequestMethod.GET)
	public ResponseEntity getAanvraagStatistieken(@PathVariable Long locatieId)
	{
		if (!locatieService.isLocatieIdVanHuisarts(getIngelogdeHuisarts(), locatieId))
		{
			return new ResponseEntity(HttpStatus.NOT_FOUND);
		}
		return ResponseEntity.ok(labformulierService.getAanvraagStatistiekenLocatie(locatieId));
	}

}
