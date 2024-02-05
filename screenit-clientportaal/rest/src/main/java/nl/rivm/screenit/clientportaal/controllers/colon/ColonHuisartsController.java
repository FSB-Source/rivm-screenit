package nl.rivm.screenit.clientportaal.controllers.colon;

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

import nl.rivm.screenit.clientportaal.controllers.AbstractController;
import nl.rivm.screenit.clientportaal.mappers.HuisartsMapper;
import nl.rivm.screenit.clientportaal.model.HuisartsDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.colon.ColonHuisartsService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("huisarts/colon")
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ColonHuisartsController extends AbstractController
{
	private final HibernateService hibernateService;

	private final ClientContactService clientContactService;

	private final ColonHuisartsService colonHuisartsService;

	private final HuisartsMapper huisartsMapper;

	@PostMapping
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> koppelColonHuisarts(Authentication authentication, @RequestParam long id)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_HUISARTS_WIJZIGEN))
		{
			EnovationHuisarts huisarts = hibernateService.get(EnovationHuisarts.class, id);

			if (client != null && client.getColonDossier() != null && huisarts != null && !huisarts.isVerwijderd())
			{
				boolean isGekoppeld = colonHuisartsService.koppelHuisarts(huisarts, client.getColonDossier().getLaatsteScreeningRonde(),
					client);
				if (isGekoppeld)
				{
					return ResponseEntity.ok().build();
				}
				return ResponseEntity.unprocessableEntity().build();
			}
			return ResponseEntity.notFound().build();
		}
		return createForbiddenResponse();
	}

	@DeleteMapping
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> ontkoppelColonHuisarts(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_HUISARTS_WIJZIGEN))
		{
			if (client != null && client.getColonDossier() != null)
			{
				boolean isOntkoppeld = colonHuisartsService.ontkoppelHuisarts(client.getColonDossier().getLaatsteScreeningRonde(), client);
				if (isOntkoppeld)
				{
					return ResponseEntity.ok().build();
				}
				return ResponseEntity.unprocessableEntity().build();
			}
			return ResponseEntity.notFound().build();
		}
		return createForbiddenResponse();
	}

	@GetMapping(path = "/vorige")
	public ResponseEntity<HuisartsDto> getVorigeColonHuisarts(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (client != null && client.getColonDossier() != null)
		{
			EnovationHuisarts huisartsVanVorigeRonde = colonHuisartsService
				.getActieveHuisartsVanVorigeRonde(client.getColonDossier().getLaatsteScreeningRonde());
			return ResponseEntity.ok(huisartsMapper.huisartsToDto(huisartsVanVorigeRonde));
		}
		return ResponseEntity.notFound().build();
	}

	@GetMapping(path = "/huidige")
	public ResponseEntity<HuisartsDto> getHuidigeColonHuisarts(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);
		if (client != null && client.getColonDossier() != null)
		{
			ColonScreeningRonde laatsteScreeningRonde = client.getColonDossier().getLaatsteScreeningRonde();
			EnovationHuisarts huisartsVanHuidigeRonde = colonHuisartsService.getActieveHuisartsVanRonde(laatsteScreeningRonde);
			return ResponseEntity.ok(huisartsMapper.huisartsToDto(huisartsVanHuidigeRonde));
		}
		return ResponseEntity.notFound().build();
	}

	@PostMapping(path = "/vorige")
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> bevestigVorigeColonHuisarts(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_HUISARTS_WIJZIGEN))
		{
			if (client != null)
			{
				boolean isBevestigd = colonHuisartsService.bevestigVorigeColonHuisarts(client, client.getColonDossier().getLaatsteScreeningRonde());
				if (isBevestigd)
				{
					return ResponseEntity.ok().build();
				}
				return ResponseEntity.unprocessableEntity().build();
			}
			return ResponseEntity.notFound().build();
		}
		return createForbiddenResponse();
	}

}
