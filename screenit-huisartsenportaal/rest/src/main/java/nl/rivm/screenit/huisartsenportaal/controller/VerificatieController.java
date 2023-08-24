package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import java.util.List;

import javax.validation.Valid;

import nl.rivm.screenit.huisartsenportaal.dto.VerificatieLocatieDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerificatieStatusDto;
import nl.rivm.screenit.huisartsenportaal.service.LocatieService;
import nl.rivm.screenit.huisartsenportaal.service.LocatieVerificatieService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("verificatie")
@PreAuthorize("isAuthenticated()")
public class VerificatieController extends BaseController
{

	@Autowired
	private LocatieVerificatieService verificatieService;

	@Autowired
	private LocatieService locatieService;

	@RequestMapping(method = RequestMethod.GET, value = "/locaties")
	public List<VerificatieLocatieDto> getLocaties()
	{
		return verificatieService.getTeVerifierenLocaties(getIngelogdeHuisarts());
	}

	@RequestMapping(method = RequestMethod.POST, value = "/verifieerLocatie")
	public ResponseEntity verifieerHuisartsLocatieCode(@Valid @RequestBody VerificatieLocatieDto locatieDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			return ResponseEntity.badRequest().body(result.getAllErrors());
		}
		VerificatieStatusDto verificatieStatus = verificatieService.verifieerLocatie(locatieDto);
		if (verificatieStatus.getSucces())
		{
			return ResponseEntity.ok(verificatieStatus);
		}
		else
		{
			return ResponseEntity.badRequest().body(verificatieStatus);
		}
	}

	@RequestMapping(method = RequestMethod.POST, value = "/herzendVerificatieCode")
	public ResponseEntity herzendVerificatieCode(@Valid @RequestBody VerificatieLocatieDto locatieDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			return ResponseEntity.badRequest().body(result.getAllErrors());
		}
		locatieService.herzendVerificatieMail(getIngelogdeHuisarts(), locatieDto);
		return ResponseEntity.ok().build();
	}
}
