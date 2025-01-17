package nl.rivm.screenit.clientportaal.controllers.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.clientportaal.controllers.AbstractController;
import nl.rivm.screenit.clientportaal.mappers.colon.ColonAfspraakZoekFilterMapper;
import nl.rivm.screenit.clientportaal.model.colon.ColonAfspraakZoekFilterDto;
import nl.rivm.screenit.clientportaal.model.colon.ColonIntakeAfspraakDto;
import nl.rivm.screenit.clientportaal.model.colon.ColonVrijSlotZonderKamerDto;
import nl.rivm.screenit.clientportaal.services.colon.ColonAfspraakService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamerFilter;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.service.colon.PlanningService;
import nl.rivm.screenit.util.ExceptionConverter;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import static nl.rivm.screenit.model.enums.BriefType.COLON_INTAKE_GEWIJZIGD;

@RequestMapping("colon/afspraak")
@RestController
@Slf4j
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ColonAfspraakController extends AbstractController
{
	private final HibernateService hibernateService;

	private final ClientContactService clientContactService;

	private final ColonAfspraakService afspraakService;

	private final ColonBaseAfspraakService baseAfspraakService;

	private final PlanningService planningService;

	private final ICurrentDateSupplier dateSupplier;

	private final ColonAfspraakZoekFilterMapper colonAfspraakZoekFilterMapper;

	private static final int MAX_RESULTS_PER_SEARCH_ITERATION = 100;

	private static final int DEFAULT_RESULTS_PER_SEARCH_ITERATION = 10;

	private boolean controleerFilterWaarde(ColonAfspraakZoekFilterDto filter)
	{
		if (filter.getVanaf() == null || filter.getVanaf().isBefore(dateSupplier.getLocalDate()))
		{
			LOG.error(String.format("Datum %s ligt te ver in het verleden", filter.getVanaf()));
			return false;
		}
		if (filter.getTotEnMet() == null || filter.getTotEnMet().isBefore(dateSupplier.getLocalDate()))
		{
			LOG.error(String.format("Datum %s ligt te ver in het verleden", filter.getTotEnMet()));
			return false;
		}
		if (filter.getPagecount() < 1)
		{
			LOG.error(String.format("Pagina telling %s is niet toegestaan", filter.getPagecount()));
			return false;
		}
		if (filter.getMaxResultsPerSearchInteration() > 100)
		{
			LOG.error(String.format("Er worden te veel results %s voor een zoek iteratie gevraagd", filter.getMaxResultsPerSearchInteration()));
			return false;
		}
		return true;
	}

	@PostMapping("/zoeken")
	@ResponseStatus(HttpStatus.OK)
	public ResponseEntity<List<ColonVrijSlotZonderKamerDto>> zoekKanditaatAfspraken(Authentication authentication, @RequestBody ColonAfspraakZoekFilterDto filter)
	{
		Client client = getClient(authentication, hibernateService);
		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN)
			|| clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN))
		{
			if (controleerFilterWaarde(filter))
			{
				int resultsSearchIteration;
				VrijSlotZonderKamerFilter zoekOpFilter = colonAfspraakZoekFilterMapper.vrijSlotToColonVrijSlotZonderKamerDto(filter);

				if (filter.getMaxResultsPerSearchInteration() != null)
				{
					resultsSearchIteration = Math.min(filter.getMaxResultsPerSearchInteration(), MAX_RESULTS_PER_SEARCH_ITERATION);
				}
				else
				{
					resultsSearchIteration = DEFAULT_RESULTS_PER_SEARCH_ITERATION;
				}

				var maxPerPageTotal = filter.getPagecount() * resultsSearchIteration;
				var maxCount = planningService.getVrijeSlotenZonderKamerCount(zoekOpFilter, client);

				List<VrijSlotZonderKamer> filterResults = planningService.getVrijeSlotenZonderKamer("vanaf", true, 0,
					Math.min(maxCount, maxPerPageTotal), zoekOpFilter, client);
				List<ColonVrijSlotZonderKamerDto> resultVrijeSloten = new ArrayList<>();

				for (VrijSlotZonderKamer vrijSlot : filterResults)
				{
					resultVrijeSloten.add(afspraakService.vanVrijSlotNaarColonVrijSlot(vrijSlot));
				}
				return ResponseEntity.ok(resultVrijeSloten);
			}
			return ResponseEntity.badRequest().build();
		}
		return createForbiddenResponse();
	}

	@GetMapping("/huidig")
	public ResponseEntity<ColonIntakeAfspraakDto> getHuidigeIntakeAfspraak(Authentication authentication)
	{
		var client = getClient(authentication, hibernateService);
		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN)
			|| clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN))
		{
			var huidigeIntakeAfspraak = afspraakService.getHuidigeIntakeAfspraak(client);
			return ResponseEntity.ok(new ColonIntakeAfspraakDto(huidigeIntakeAfspraak));
		}
		return ResponseEntity.ok().build();
	}

	@PutMapping("/afzeggen")

	public ResponseEntity<Void> zegIntakeAfspraakAf(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);
		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN))
		{
			afspraakService.intakeAfspraakAfzeggen(client);
			return ResponseEntity.ok().build();
		}
		return createForbiddenResponse();
	}

	@PutMapping("/verplaatsen")

	public ResponseEntity<String> verplaatsIntakeAfspraak(Authentication authentication, @RequestBody ColonVrijSlotZonderKamerDto verplaatsAfspraak)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN))
		{
			return maakOfVerplaatsAfspraakIndienMogelijk(verplaatsAfspraak, client, ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN);
		}
		return createForbiddenResponse();
	}

	@PutMapping("/maken")

	public ResponseEntity<String> maakIntakeAfspraak(Authentication authentication, @RequestBody ColonVrijSlotZonderKamerDto maakAfspraak)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN))
		{
			return maakOfVerplaatsAfspraakIndienMogelijk(maakAfspraak, client, ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN);
		}
		return createForbiddenResponse();
	}

	private ResponseEntity<String> maakOfVerplaatsAfspraakIndienMogelijk(ColonVrijSlotZonderKamerDto verplaatsAfspraak, Client client,
		ClientContactActieType contactActieType)
	{
		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN)
			|| clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN))
		{
			ColonIntakeAfspraak vorigeAfspraak = afspraakService.getHuidigeIntakeAfspraak(client);

			if (vorigeAfspraak == null)
			{
				LOG.error("Intakeafspraak onbekend");
				return ResponseEntity.badRequest().build();
			}

			try
			{
				ColonIntakeAfspraak nieuweIntakeAfspraak = afspraakService.initNieuweAfspraak(vorigeAfspraak, verplaatsAfspraak);
				if (nieuweIntakeAfspraak != null)
				{
					if (ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN.equals(contactActieType))
					{
						baseAfspraakService.verplaatsAfspraak(nieuweIntakeAfspraak, client, COLON_INTAKE_GEWIJZIGD, false, true, false);
					}
					if (ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN.equals(contactActieType))
					{
						baseAfspraakService.maakNieuweAfspraak(client, nieuweIntakeAfspraak, false, true, null, client);
					}
				}
				return ResponseEntity.ok().build();
			}
			catch (RuntimeException e)
			{
				return ResponseEntity.status(HttpStatus.CONFLICT).body(ExceptionConverter.convertExceptionToJson(e, "afspraak.niet.gemaakt"));
			}
		}
		return createForbiddenResponse();
	}
}
