package nl.rivm.screenit.clientportaal.controllers.mamma;

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

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.clientportaal.controllers.AbstractController;
import nl.rivm.screenit.clientportaal.exception.NotValidException;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakWijzigenFilterDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakZoekFilterDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaStandplaatsperiodeOptieDto;
import nl.rivm.screenit.clientportaal.services.DatumValidatieService;
import nl.rivm.screenit.clientportaal.services.mamma.MammaAfspraakService;
import nl.rivm.screenit.clientportaal.services.mamma.MammaUitstelService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;

import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("mamma/uitstel")
@Slf4j
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class UitstelController extends AbstractController
{
	private final MammaBaseStandplaatsService standplaatsService;

	private final MammaAfspraakService afspraakService;

	private final MammaUitstelService uitstelService;

	private final DatumValidatieService datumValidatieService;

	@PostMapping("/zoeken")
	public ResponseEntity<List<MammaStandplaatsperiodeOptieDto>> zoekStandplaatsPeriodes(Authentication authentication, @RequestBody MammaAfspraakZoekFilterDto body)
	{
		if (datumValidatieService.datumIsInHetVerleden(body.getVanaf()))
		{
			LOG.error("Datum ligt in het verleden");
			return ResponseEntity.badRequest().build();
		}

		Client client = getClient(authentication);

		if (aanvraagIsToegestaneActie(client, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN)
			|| aanvraagIsToegestaneActie(client, ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN))
		{
			MammaAfspraakWijzigenFilterDto filter = afspraakService.toAfspraakFilter(body, client, false);

			return ResponseEntity.ok().body(standplaatsService.getStandplaatsPeriodeMetAfstandDtos(client, filter, true).stream().distinct()
				.map(standplaats -> uitstelService.toStandplaatsPeriodeOptie(standplaats, filter))
				.sorted(Comparator.comparing(MammaStandplaatsperiodeOptieDto::getAfstand)).collect(Collectors.toList()));
		}
		return createForbiddenResponse();
	}

	@PostMapping
	@Transactional(propagation = Propagation.REQUIRED)
	public ResponseEntity<String> maakUitstel(Authentication authentication, @RequestBody MammaStandplaatsperiodeOptieDto standplaatsPeriodeDto)
	{
		Client client = getClient(authentication);

		if (datumValidatieService.datumIsInHetVerleden(standplaatsPeriodeDto.getFilter().getVanaf()))
		{
			LOG.error("Datum ligt in het verleden");
			return ResponseEntity.badRequest().build();
		}

		if (!uitstelService.beschikbareStandplaatsperiodesBevatGekozenStandplaatsperiode(standplaatsPeriodeDto, client))
		{
			LOG.error("De gekozen standplaatsperiode is niet beschikbaar");
			return ResponseEntity.badRequest().build();
		}

		if (aanvraagIsToegestaneActie(client, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN)
			|| aanvraagIsToegestaneActie(client, ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN))
		{
			try
			{
				uitstelService.maakUitstelEnSlaOp(client, standplaatsPeriodeDto);
				return ResponseEntity.ok().build();
			}
			catch (NotValidException exception)
			{
				return ResponseEntity.badRequest().body(exception.getMessage());
			}
		}
		return createForbiddenResponse();
	}
}
