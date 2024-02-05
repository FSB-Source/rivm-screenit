package nl.rivm.screenit.clientportaal.controllers.mamma;

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
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.mamma.MammaHuisartsService;
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
@RequestMapping("huisarts/mamma")
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaHuisartsController extends AbstractController
{
	private final HibernateService hibernateService;

	private final ClientContactService clientContactService;

	private final MammaHuisartsService mammaHuisartsService;

	private final HuisartsMapper huisartsMapper;

	@PostMapping
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> koppelMammaHuisarts(Authentication authentication, @RequestParam long id)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN))
		{
			EnovationHuisarts huisarts = hibernateService.get(EnovationHuisarts.class, id);

			if (client != null && huisarts != null && !huisarts.isVerwijderd())
			{
				boolean isGekoppeld = mammaHuisartsService.koppelHuisarts(huisarts, client.getMammaDossier().getLaatsteScreeningRonde(), client);
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
	public ResponseEntity<Void> ontkoppelMammaHuisarts(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN))
		{
			if (client != null)
			{
				MammaDossier dossier = client.getMammaDossier();
				if (dossier != null)
				{
					MammaScreeningRonde laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();
					if (laatsteScreeningRonde != null)
					{
						if (mammaHuisartsService.magHuisartsVerwijderen(laatsteScreeningRonde)
							&& mammaHuisartsService.ontkoppelHuisarts(laatsteScreeningRonde, client))
						{
							return ResponseEntity.ok().build();
						}
						return ResponseEntity.unprocessableEntity().build();
					}
				}
			}
			return ResponseEntity.notFound().build();
		}
		return createForbiddenResponse();
	}

	@GetMapping(path = "/magverwijderen")
	public ResponseEntity<Boolean> magMammaHuisartsVerwijderen(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN))
		{
			if (client != null && client.getMammaDossier() != null)
			{
				MammaScreeningRonde laatsteScreeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
				if (laatsteScreeningRonde != null)
				{
					return ResponseEntity.ok(mammaHuisartsService.magHuisartsVerwijderen(laatsteScreeningRonde));
				}
				return ResponseEntity.ok(false);
			}
			return ResponseEntity.notFound().build();
		}
		return createForbiddenResponse();
	}

	@GetMapping(path = "/vorige")
	public ResponseEntity<HuisartsDto> getVorigeMammaHuisarts(Authentication authentication)
	{

		Client client = getClient(authentication, hibernateService);
		if (client != null && client.getMammaDossier() != null)
		{
			MammaScreeningRonde laatsteScreeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
			EnovationHuisarts huisartsVanVorigeRonde = mammaHuisartsService.getActieveHuisartsVanVorigeRonde(laatsteScreeningRonde);
			return ResponseEntity.ok(huisartsMapper.huisartsToDto(huisartsVanVorigeRonde));
		}
		return ResponseEntity.notFound().build();
	}

	@GetMapping(path = "/huidige")
	public ResponseEntity<HuisartsDto> getHuidigeMammaHuisarts(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);
		if (client != null && client.getMammaDossier() != null)
		{
			MammaScreeningRonde laatsteScreeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
			if (laatsteScreeningRonde != null)
			{
				EnovationHuisarts huisartsVanHuidigeRonde = mammaHuisartsService.getActieveHuisartsVanRonde(laatsteScreeningRonde);
				return ResponseEntity.ok(huisartsMapper.huisartsToDto(huisartsVanHuidigeRonde));
			}
		}
		return ResponseEntity.notFound().build();
	}

	@GetMapping(path = "/vorige/geen")
	public ResponseEntity<MammaGeenHuisartsOption> getVorigeMammaGeenHuisartsOptie(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);
		if (client != null && client.getMammaDossier() != null)
		{
			MammaScreeningRonde laatsteScreeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
			MammaGeenHuisartsOption geenHuisartsOptie = mammaHuisartsService.getMammaGeenHuisartsOptieVorigeRonde(laatsteScreeningRonde);
			return ResponseEntity.ok(geenHuisartsOptie);
		}
		return ResponseEntity.notFound().build();
	}

	@GetMapping(path = "/huidige/geen")
	public ResponseEntity<MammaGeenHuisartsOption> getHuidigeMammaGeenHuisartsOptie(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);
		if (client != null && client.getMammaDossier() != null)
		{
			MammaScreeningRonde laatsteScreeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
			if (laatsteScreeningRonde != null)
			{
				MammaGeenHuisartsOption geenHuisartsOptie = mammaHuisartsService.getMammaGeenHuisartsOptieVanRonde(laatsteScreeningRonde);
				return ResponseEntity.ok(geenHuisartsOptie);
			}
		}
		return ResponseEntity.notFound().build();
	}

	@PostMapping(path = "/vorige")
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<Void> bevestigVorigeMammaHuisarts(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN))
		{
			if (client != null && client.getMammaDossier() != null)
			{
				MammaScreeningRonde laatsteScreeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
				if (laatsteScreeningRonde != null)
				{
					boolean isBevestigd = mammaHuisartsService.bevestigVorigeMammaHuisartsKeuze(client, laatsteScreeningRonde);
					if (isBevestigd)
					{
						return ResponseEntity.ok().build();
					}
					return ResponseEntity.unprocessableEntity().build();
				}
			}
			return ResponseEntity.notFound().build();
		}
		return createForbiddenResponse();
	}

}
