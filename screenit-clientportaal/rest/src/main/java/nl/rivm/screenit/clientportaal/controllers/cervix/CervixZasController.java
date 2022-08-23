package nl.rivm.screenit.clientportaal.controllers.cervix;

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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.clientportaal.controllers.AbstractController;
import nl.rivm.screenit.clientportaal.model.cervix.CervixZasStatusDto;
import nl.rivm.screenit.clientportaal.services.cervix.CervixZasService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;

import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RequestMapping("cervix/zas")
@RestController
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@Slf4j
@AllArgsConstructor
public class CervixZasController extends AbstractController
{

	private final CervixZasService zasService;

	@GetMapping("status")
	public ResponseEntity<CervixZasStatusDto> getZasStatus(Authentication authentication)
	{
		Client client = getClient(authentication);

		if (aanvraagIsToegestaneActie(client, ClientContactActieType.CERVIX_ZAS_AANVRAGEN))
		{
			return ResponseEntity.ok(zasService.getZasStatus(client));
		}
		return createForbiddenResponse();
	}

	@PostMapping("aanvragen/{ontvangenNaUitstel}")
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> vraagZasAan(@PathVariable Boolean ontvangenNaUitstel, Authentication authentication)
	{
		Client client = getClient(authentication);

		if (aanvraagIsToegestaneActie(client, ClientContactActieType.CERVIX_ZAS_AANVRAGEN))
		{
			CervixScreeningRonde laatsteRonde = client.getCervixDossier().getLaatsteScreeningRonde();
			if (!ontvangenNaUitstel)
			{
				zasService.vraagZasAan(getClient(authentication), false);
				return ResponseEntity.ok().build();
			}
			else if (laatsteRonde != null && zasService.rondeHeeftCervixUitstel(laatsteRonde))
			{
				zasService.vraagZasAan(getClient(authentication), true);
				return ResponseEntity.ok().build();
			}
			else
			{
				LOG.error("Cliënt '{}' heeft geen cervix uitstel", client.getId());
				return ResponseEntity.badRequest().build();
			}
		}
		return createForbiddenResponse();
	}

}
