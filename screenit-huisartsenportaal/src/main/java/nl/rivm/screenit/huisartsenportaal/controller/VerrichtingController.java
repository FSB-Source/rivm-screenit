package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingCsvDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingTotalenDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingZoekObjectDto;
import nl.rivm.screenit.huisartsenportaal.service.VerrichtingenService;
import nl.rivm.screenit.huisartsenportaal.validator.VerrichtingenValidator;

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
@RequestMapping("/api/v1/verrichting")
@PreAuthorize("isAuthenticated()")
public class VerrichtingController extends BaseController
{
	@Autowired
	private VerrichtingenService verrichtingenService;

	@Autowired
	private VerrichtingenValidator verrichtingenValidator;

	@InitBinder
	public void dataBinding(WebDataBinder binder)
	{

		binder.addValidators(verrichtingenValidator);
	}

	@RequestMapping(method = RequestMethod.POST, path = "/all")
	@PreAuthorize("hasRole('ROLE_AANVRAGEN')")
	public ResponseEntity getHuidigeVerrichtingen(@Valid @RequestBody VerrichtingZoekObjectDto verrichtingDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			return ResponseEntity.badRequest().body(result.getAllErrors());
		}
		VerrichtingTotalenDto verrichtingDtos = verrichtingenService.getVerrichtingen(getIngelogdeHuisarts(), verrichtingDto);
		return new ResponseEntity(verrichtingDtos, HttpStatus.OK);
	}

	@RequestMapping(method = RequestMethod.POST, path = "/csv")
	@PreAuthorize("hasRole('ROLE_AANVRAGEN')")
	public ResponseEntity getHuidigeVerrichtingenCsv(@Valid @RequestBody VerrichtingZoekObjectDto verrichtingDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			return ResponseEntity.badRequest().body(result.getAllErrors());
		}
		List<VerrichtingCsvDto> verrichtingDtos = verrichtingenService.getVerrichtingenCsv(getIngelogdeHuisarts(), verrichtingDto);
		return new ResponseEntity(verrichtingDtos, HttpStatus.OK);
	}
}
