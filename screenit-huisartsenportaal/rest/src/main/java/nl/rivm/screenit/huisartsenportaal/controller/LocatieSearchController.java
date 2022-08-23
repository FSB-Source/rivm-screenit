package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.huisartsenportaal.dto.locatie.LocatieSearchDto;
import nl.rivm.screenit.huisartsenportaal.service.LocatieService;
import nl.rivm.screenit.huisartsenportaal.validator.LocatieSearchValidator;

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
@RequestMapping("locaties")
@PreAuthorize("isAuthenticated()")
public class LocatieSearchController extends BaseController
{
	@Autowired
	private LocatieSearchValidator locatieSearchValidator;

	@Autowired
	private LocatieService locatieService;

	@InitBinder
	public void dataBinding(WebDataBinder binder)
	{
		binder.addValidators(locatieSearchValidator);
	}

	@RequestMapping(method = RequestMethod.POST)
	public ResponseEntity getLocaties(@Valid @RequestBody LocatieSearchDto searchDto, BindingResult result)
	{
		var arts = getIngelogdeHuisarts();
		if (arts != null)
		{
			var dto = locatieService.getLocatieResultDto(arts, searchDto);
			return new ResponseEntity<>(dto, HttpStatus.OK);
		}
		return ResponseEntity.badRequest().body("Er is iets misgegaan.");
	}
}
