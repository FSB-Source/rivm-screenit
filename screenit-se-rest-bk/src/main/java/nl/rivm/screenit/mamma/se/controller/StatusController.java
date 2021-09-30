package nl.rivm.screenit.mamma.se.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import javax.servlet.http.HttpServletRequest;

import nl.rivm.screenit.mamma.se.dto.SeStatusDto;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidStatusService;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("status")
public class StatusController extends AuthorizedController
{

	@Autowired
	private MammaScreeningsEenheidStatusService statusService;

	@RequestMapping(method = RequestMethod.POST)
	public ResponseEntity statusMelden(@RequestBody SeStatusDto statusDto, HttpServletRequest request)
	{
		MammaScreeningsEenheid screeningsEenheid = getScreeningsEenheid(request);
		statusService.verwerkStatusBericht(screeningsEenheid, statusDto);
		return ResponseEntity.ok().build();
	}
}
