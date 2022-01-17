package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.validation.Valid;

import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.service.LocatieService;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;
import nl.rivm.screenit.huisartsenportaal.validator.LocatieValidator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/locatie")
@PreAuthorize("isAuthenticated()")
public class LocatieController extends BaseController
{
	@Autowired
	private LocatieValidator locatieValidator;

	@Autowired
	private LocatieService locatieService;

	@Autowired
	private SynchronisatieService syncService;

	@InitBinder
	public void dataBinding(WebDataBinder binder)
	{

		binder.addValidators(locatieValidator);
	}

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity getLocaties()
	{
		Huisarts arts = getIngelogdeHuisarts();
		if (arts != null)
		{
			List<LocatieDto> locatieDtos = locatieService.getAllLocatiesFromHuisartsInDto(arts);
			return new ResponseEntity<>(locatieDtos, HttpStatus.OK);
		}
		return ResponseEntity.badRequest().body("Er is iets misgegaan.");
	}

	@RequestMapping(method = RequestMethod.POST)
	public ResponseEntity saveLocatie(@Valid @RequestBody LocatieDto locatieDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			return ResponseEntity.badRequest().body(result.getAllErrors());
		}

		Huisarts arts = getIngelogdeHuisarts();
		Locatie locatie = locatieService.setLocatie(arts, locatieDto);
		syncService.syncLocatie(arts, locatie, locatieDto.getHerzendVerificatieMail());
		List<LocatieDto> locatieDtos = locatieService.getAllLocatiesFromHuisartsInDto(arts);
		return new ResponseEntity<>(locatieDtos, HttpStatus.OK);
	}

	@RequestMapping(method = RequestMethod.PUT)
	public ResponseEntity putLocatie(@Valid @RequestBody LocatieDto locatieDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			return ResponseEntity.badRequest().body(result.getAllErrors());
		}

		Huisarts arts = getIngelogdeHuisarts();
		Locatie locatie = locatieService.setLocatie(arts, locatieDto);
		syncService.syncLocatie(arts, locatie, locatieDto.getHerzendVerificatieMail());
		return ResponseEntity.ok().body(locatieService.getLocatieDto(locatie));
	}
}
