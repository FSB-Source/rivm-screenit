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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.clientportaal.mappers.BezwaarMapper;
import nl.rivm.screenit.clientportaal.model.BezwaarDto;
import nl.rivm.screenit.clientportaal.services.BezwaarValidatieService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.service.BezwaarService;
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
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("bezwaar")
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class BezwaarController extends AbstractController
{
	private final HibernateService hibernateService;

	private final ClientContactService clientContactService;

	private final BezwaarService bezwaarService;

	private final BezwaarValidatieService bezwaarValidatieService;

	private final BezwaarMapper mapper;

	@GetMapping(value = "/{bevolkingsonderzoek}")
	public ResponseEntity<List<BezwaarDto>> getBezwaren(@PathVariable Bevolkingsonderzoek bevolkingsonderzoek, Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.BEZWAAR))
		{
			bezwaarValidatieService.valideerOfClientBehoortTotDoelgroep(client, bevolkingsonderzoek);

			List<BezwaarGroupViewWrapper> bezwaarGroupViewWrappers = bezwaarService
				.getGroupWrapperForClientPortaal(client.getLaatstVoltooideBezwaarMoment(), bevolkingsonderzoek);

			return ResponseEntity.ok(wrappersToDto(bezwaarGroupViewWrappers, bevolkingsonderzoek));
		}
		return createForbiddenResponse();
	}

	private List<BezwaarDto> wrappersToDto(List<BezwaarGroupViewWrapper> bezwaarGroupViewWrappers, Bevolkingsonderzoek bevolkingsonderzoek)
	{
		return mapper.bezwarenToDtos(bezwaarGroupViewWrappers.stream()
			.filter(b -> b.getBevolkingsonderzoek() == null || b.getBevolkingsonderzoek() == bevolkingsonderzoek)
			.flatMap(bm -> bm.getBezwaren().stream()).collect(Collectors.toList()));
	}

	@PostMapping(value = "/{bevolkingsonderzoek}")
	public ResponseEntity<List<BezwaarDto>> saveBezwaren(@PathVariable Bevolkingsonderzoek bevolkingsonderzoek, @RequestBody BezwaarDto[] bezwaarDtos,
		Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.BEZWAAR))
		{
			bezwaarValidatieService.valideerOfClientBehoortTotDoelgroep(client, bevolkingsonderzoek);
			bezwaarValidatieService.valideerBezwaarType(bezwaarDtos);

			List<BezwaarGroupViewWrapper> bezwaarGroupViewWrappers = getBezwaarGroupViewWrappersVoorAlleBvos(client);

			updateWrappersMetWijzigingenDoorClient(bevolkingsonderzoek, bezwaarDtos, bezwaarGroupViewWrappers);
			boolean bezwarenGewijzigd = bezwaarService.bezwarenGewijzigd(client.getLaatstVoltooideBezwaarMoment(), bezwaarGroupViewWrappers, bevolkingsonderzoek);
			if (!bezwarenGewijzigd)
			{
				return ResponseEntity.status(HttpStatus.NOT_MODIFIED).build();
			}
			bezwaarService.bezwaarAfrondenVanuitClientPortaal(client, bezwaarGroupViewWrappers);
			return ResponseEntity.ok(wrappersToDto(bezwaarGroupViewWrappers, bevolkingsonderzoek));

		}
		return createForbiddenResponse();
	}

	private List<BezwaarGroupViewWrapper> getBezwaarGroupViewWrappersVoorAlleBvos(Client client)
	{
		List<BezwaarGroupViewWrapper> bezwaarGroupViewWrappers = new ArrayList<>();
		Arrays.stream(Bevolkingsonderzoek.values())
			.forEach(
				bvo -> bezwaarGroupViewWrappers
					.addAll(bezwaarService.getGroupWrapperForClientPortaal(client.getLaatstVoltooideBezwaarMoment(), bvo)));
		return bezwaarGroupViewWrappers;
	}

	private void updateWrappersMetWijzigingenDoorClient(Bevolkingsonderzoek bevolkingsonderzoek, BezwaarDto[] bezwaarDtos,
		List<BezwaarGroupViewWrapper> bezwaarGroupViewWrappers)
	{
		bezwaarGroupViewWrappers.stream()
			.filter(g -> g.getBevolkingsonderzoek() == null || g.getBevolkingsonderzoek() == bevolkingsonderzoek)
			.flatMap(g -> g.getBezwaren().stream())
			.forEach(b -> Arrays.stream(bezwaarDtos)
				.filter(bd -> bd.getType() == b.getType())
				.forEach(bd -> b.setActief(bd.getActive())));
	}

}
