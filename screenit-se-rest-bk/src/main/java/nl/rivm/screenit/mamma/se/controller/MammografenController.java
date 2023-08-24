package nl.rivm.screenit.mamma.se.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.util.Collection;

import javax.servlet.http.HttpServletRequest;

import nl.rivm.screenit.mamma.se.dto.MammograafDto;
import nl.rivm.screenit.mamma.se.service.MammografenService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/mammografen")
public class MammografenController extends AuthorizedController
{
	private static final Logger LOG = LoggerFactory.getLogger(MammografenController.class);

	@Autowired
	private MammografenService mammografenService;

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity readMammografen(HttpServletRequest request)
	{
		try
		{
			Collection<MammograafDto> mammografen = mammografenService.getMammografen(getSeCode(request));
			return ResponseEntity.ok(mammografen);
		}
		catch (Exception ex)
		{
			return createErrorResponse(ex);
		}
	}
}
