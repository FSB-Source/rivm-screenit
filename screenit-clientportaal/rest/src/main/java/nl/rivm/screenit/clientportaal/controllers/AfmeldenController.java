package nl.rivm.screenit.clientportaal.controllers;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.clientportaal.model.AfmeldOptiesDto;
import nl.rivm.screenit.clientportaal.model.AfmeldingDto;
import nl.rivm.screenit.clientportaal.services.AfmeldenService;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.enums.CervixAfmeldingReden;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.enums.MammaAfmeldingReden;
import nl.rivm.screenit.service.BaseAfmeldService;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

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
@Slf4j
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class AfmeldenController extends AbstractController
{
	private final HibernateService hibernateService;

	private final ClientContactService clientContactService;

	private final BaseAfmeldService baseAfmeldService;

	private final AfmeldenService afmeldenService;

	private final LogService logService;

	@GetMapping(value = "/{bevolkingsonderzoek}")
	public ResponseEntity<AfmeldOptiesDto> getAfmeldOpties(@PathVariable Bevolkingsonderzoek bevolkingsonderzoek, Authentication authentication)
	{
		return ResponseEntity.ok(afmeldenService.getAfmeldOpties(getClient(authentication, hibernateService), bevolkingsonderzoek));
	}

	@PostMapping(value = "/cervix")
	@ResponseStatus(HttpStatus.OK)
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> saveCervixAfmelding(@RequestBody AfmeldingDto<CervixAfmeldingReden> afmeldingDto, Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.CERVIX_AFMELDEN))
		{
			CervixAfmelding cervixAfmelding = afmeldenService.valideerEnGetCervixAfmelding(afmeldingDto, client);
			handleAfmelding(client, cervixAfmelding);
			return ResponseEntity.ok().build();
		}
		return createForbiddenResponse();
	}

	@PostMapping(value = "/colon")
	@ResponseStatus(HttpStatus.OK)
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> saveColonAfmelding(@RequestBody AfmeldingDto<ColonAfmeldingReden> afmeldingDto, Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_AFMELDEN))
		{
			ColonAfmelding colonAfmelding = afmeldenService.valideerEnGetColonAfmelding(afmeldingDto, client);
			handleAfmelding(client, colonAfmelding);
			return ResponseEntity.ok().build();
		}
		return createForbiddenResponse();
	}

	@PostMapping(value = "/mamma")
	@ResponseStatus(HttpStatus.OK)
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> saveMammaAfmelding(@RequestBody AfmeldingDto<MammaAfmeldingReden> afmeldingDto, Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_AFMELDEN))
		{
			MammaAfmelding mammaAfmelding = afmeldenService.valideerEnGetMammaAfmelding(afmeldingDto, client);
			handleAfmelding(client, mammaAfmelding);
			return ResponseEntity.ok().build();
		}
		return createForbiddenResponse();
	}

	private void handleAfmelding(Client client, Afmelding<?, ?, ?> afmelding)
	{
		baseAfmeldService.afmelden(client, afmelding, client);
		logService.logGebeurtenis(LogGebeurtenis.AFMELDEN, client, "Type: " + afmelding.getType().name().toLowerCase(), afmelding.getBevolkingsonderzoek());
	}
}
