package nl.rivm.screenit.clientportaal.controllers;

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

import nl.rivm.screenit.clientportaal.model.AfmeldOptiesDto;
import nl.rivm.screenit.clientportaal.model.AfmeldingDto;
import nl.rivm.screenit.clientportaal.services.AfmeldenService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.enums.MammaAfmeldingReden;
import nl.rivm.screenit.service.BaseAfmeldService;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("afmelden")
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class AfmeldenController extends AbstractController
{
	private final BaseAfmeldService baseAfmeldService;

	private final AfmeldenService afmeldenService;

	@GetMapping(value = "/{bevolkingsonderzoek}")
	public ResponseEntity<AfmeldOptiesDto> getAfmeldOpties(@PathVariable Bevolkingsonderzoek bevolkingsonderzoek, Authentication authentication)
	{
		return ResponseEntity.ok(afmeldenService.getAfmeldOpties(getClient(authentication), bevolkingsonderzoek));
	}

	@PostMapping(value = "/cervix")
	@ResponseStatus(HttpStatus.OK)
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> saveCervixAfmelding(@RequestBody AfmeldingDto<CervixAfmeldingReden> afmeldingDto, Authentication authentication)
	{
		Client client = getClient(authentication);

		if (aanvraagIsToegestaneActie(client, ClientContactActieType.CERVIX_AFMELDEN))
		{
			CervixAfmelding cervixAfmelding = afmeldenService.valideerEnGetCervixAfmelding(afmeldingDto, client);
			baseAfmeldService.afmelden(client, cervixAfmelding, client);
			return ResponseEntity.ok().build();
		}
		return createForbiddenResponse();
	}

	@PostMapping(value = "/colon")
	@ResponseStatus(HttpStatus.OK)
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> saveColonAfmelding(@RequestBody AfmeldingDto<ColonAfmeldingReden> afmeldingDto, Authentication authentication)
	{
		Client client = getClient(authentication);

		if (aanvraagIsToegestaneActie(client, ClientContactActieType.COLON_AFMELDEN))
		{
			ColonAfmelding colonAfmelding = afmeldenService.valideerEnGetColonAfmelding(afmeldingDto, client);
			baseAfmeldService.afmelden(client, colonAfmelding, client);
			return ResponseEntity.ok().build();
		}
		return createForbiddenResponse();
	}

	@PostMapping(value = "/mamma")
	@ResponseStatus(HttpStatus.OK)
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> saveMammaAfmelding(@RequestBody AfmeldingDto<MammaAfmeldingReden> afmeldingDto, Authentication authentication)
	{
		Client client = getClient(authentication);

		if (aanvraagIsToegestaneActie(client, ClientContactActieType.MAMMA_AFMELDEN))
		{
			MammaAfmelding mammaAfmelding = afmeldenService.valideerEnGetMammaAfmelding(afmeldingDto, client);
			baseAfmeldService.afmelden(client, mammaAfmelding, client);
			return ResponseEntity.ok().build();
		}
		return createForbiddenResponse();
	}
}
