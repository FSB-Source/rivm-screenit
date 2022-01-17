package nl.rivm.screenit.clientportaal.controllers.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import nl.rivm.screenit.clientportaal.controllers.AbstractController;
import nl.rivm.screenit.clientportaal.model.colon.ColonFitStatusDto;
import nl.rivm.screenit.clientportaal.services.colon.ColonFitService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

@RequestMapping("colon/fit")
@RestController
public class ColonFitAanvragenController extends AbstractController
{
	@Autowired
	private ColonFitService fitService;

	@GetMapping("status")
	public ResponseEntity<ColonFitStatusDto> getFitStatus(Authentication authentication)
	{
		Client client = getClient(authentication);

		if (aanvraagIsToegestaneActie(client, ClientContactActieType.COLON_AANVRAGEN_NIEUWE_IFOBT))
		{
			return ResponseEntity.ok(fitService.getFitStatus(client));
		}
		return createForbiddenResponse();
	}

	@PutMapping("aanvragen")
	public ResponseEntity<Void> vraagFitAan(Authentication authentication)
	{
		Client client = getClient(authentication);

		if (aanvraagIsToegestaneActie(client, ClientContactActieType.COLON_AANVRAGEN_NIEUWE_IFOBT))
		{
			fitService.vraagFitAan(client);
			return ResponseEntity.ok().build();
		}
		return createForbiddenResponse();
	}
}
