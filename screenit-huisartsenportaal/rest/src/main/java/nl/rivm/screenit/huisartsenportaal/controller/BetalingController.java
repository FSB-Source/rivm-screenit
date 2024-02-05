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

import java.io.FileInputStream;

import javax.validation.Valid;

import lombok.SneakyThrows;

import nl.rivm.screenit.huisartsenportaal.dto.BetalingZoekObjectDto;
import nl.rivm.screenit.huisartsenportaal.dto.BetalingenTotalenDto;
import nl.rivm.screenit.huisartsenportaal.service.BetalingService;
import nl.rivm.screenit.huisartsenportaal.validator.BetalingenValidator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
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
@RequestMapping("betaling")
@PreAuthorize("isAuthenticated()")
public class BetalingController extends BaseController
{
	@Autowired
	private BetalingService betalingService;

	@Autowired
	private BetalingenValidator betalingenValidator;

	@InitBinder
	public void dataBinding(WebDataBinder binder)
	{

		binder.addValidators(betalingenValidator);
	}

	@RequestMapping(method = RequestMethod.POST, path = "/all")
	@PreAuthorize("hasRole('ROLE_AANVRAGEN')")
	public ResponseEntity getHuidigeBetalingen(@Valid @RequestBody BetalingZoekObjectDto betalingZoekObjectDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			return ResponseEntity.badRequest().body(result.getAllErrors());
		}
		BetalingenTotalenDto betalingenTotalenDto = betalingService.getBetalingen(getIngelogdeHuisarts(), betalingZoekObjectDto);
		return new ResponseEntity(betalingenTotalenDto, HttpStatus.OK);
	}

	@SneakyThrows
	@RequestMapping(method = RequestMethod.POST, path = "/csv", produces = MediaType.APPLICATION_OCTET_STREAM_VALUE)
	@PreAuthorize("hasRole('ROLE_AANVRAGEN')")
	public ResponseEntity<Resource> getHuidigeBetalingenCsv(@Valid @RequestBody BetalingZoekObjectDto betalingZoekObjectDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			return (ResponseEntity<Resource>) ResponseEntity.badRequest();
		}

		var betalingenCsv = betalingService.getBetalingenCsv(getIngelogdeHuisarts(), betalingZoekObjectDto);

		return ResponseEntity.ok()
			.body(new InputStreamResource(new FileInputStream(betalingenCsv)));
	}
}
