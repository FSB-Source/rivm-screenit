package nl.rivm.screenit.clientportaal.controllers;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.model.HeraanmeldenOptiesDto;
import nl.rivm.screenit.clientportaal.services.HeraanmeldenService;
import nl.rivm.screenit.exceptions.MammaStandplaatsVanPostcodeOnbekendException;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.ClientContactService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("heraanmelden")
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class HeraanmeldenController extends AbstractController
{
	private final HibernateService hibernateService;

	private final ClientContactService clientContactService;

	private final HeraanmeldenService heraanmeldenService;

	@GetMapping(value = "/{bevolkingsonderzoek}")
	public ResponseEntity<HeraanmeldenOptiesDto> getHeraanmeldStatus(@PathVariable Bevolkingsonderzoek bevolkingsonderzoek, Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, getClientContactActieType(bevolkingsonderzoek)))
		{
			boolean magColonUitnodigingAanvragen = false;
			boolean magColonIntakAfspraakInplannen = false;

			if (Bevolkingsonderzoek.COLON.equals(bevolkingsonderzoek))
			{
				ColonDossier colonDossier = client.getColonDossier();
				magColonUitnodigingAanvragen = clientContactService.magNieuweUitnodigingAanvragen(colonDossier, true);
				magColonIntakAfspraakInplannen = clientContactService.magNieuweIntakeAfspraakMakenNaHeraanmelding(colonDossier);
			}
			return ResponseEntity.ok(new HeraanmeldenOptiesDto(magColonUitnodigingAanvragen, magColonIntakAfspraakInplannen));
		}
		return createForbiddenResponse();
	}

	@PostMapping(value = "/{bevolkingsonderzoek}/{wilNieuweUitnodigingOntvangen}")
	public ResponseEntity<String> saveHeraanmeldVerzoek(@PathVariable Bevolkingsonderzoek bevolkingsonderzoek, @PathVariable boolean wilNieuweUitnodigingOntvangen,
		Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);
		try
		{
			if (clientContactService.availableActiesBevatBenodigdeActie(client, getClientContactActieType(bevolkingsonderzoek)))
			{
				heraanmeldenService.saveHeraanmeldenVerzoek(getClient(authentication, hibernateService), bevolkingsonderzoek, wilNieuweUitnodigingOntvangen);
				return ResponseEntity.ok().build();
			}
		}
		catch (MammaStandplaatsVanPostcodeOnbekendException e)
		{
			return ResponseEntity.status(HttpStatus.CONFLICT).body("mamma.standplaats.postcode.onbekend");
		}
		return createForbiddenResponse();
	}

	private ClientContactActieType getClientContactActieType(Bevolkingsonderzoek bevolkingsonderzoek)
	{
		switch (bevolkingsonderzoek)
		{
		case COLON:
			return ClientContactActieType.COLON_HERAANMELDEN;
		case MAMMA:
			return ClientContactActieType.MAMMA_HERAANMELDEN;
		case CERVIX:
			return ClientContactActieType.CERVIX_HERAANMELDEN;
		default:
			return null;
		}
	}
}
