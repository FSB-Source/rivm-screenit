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

import java.time.LocalDate;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.clientportaal.controllers.AbstractController;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakBevestigingDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakOptieDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakWijzigenFilterDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaAfspraakZoekFilterDto;
import nl.rivm.screenit.clientportaal.model.mamma.MammaBeschikbaarheidPlaatsOpvragenDto;
import nl.rivm.screenit.clientportaal.services.DatumValidatieService;
import nl.rivm.screenit.clientportaal.services.mamma.MammaAfspraakService;
import nl.rivm.screenit.dto.mamma.afspraken.MammaHuidigeAfspraakDto;
import nl.rivm.screenit.exceptions.MammaTijdNietBeschikbaarException;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.enums.BevestigingsType;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.enums.SmsStatus;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EmailUtil;
import nl.rivm.screenit.util.TelefoonnummerUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.jetbrains.annotations.NotNull;
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
@RequestMapping("mamma/afspraak")
@Slf4j
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class AfspraakController extends AbstractController
{
	private final HibernateService hibernateService;

	private final ClientContactService clientContactService;

	private final MammaBaseStandplaatsService standplaatsService;

	private final MammaBaseAfspraakService baseAfspraakService;

	private final MammaAfspraakService afspraakService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final DatumValidatieService datumValidatieService;

	@GetMapping("/standplaatsPlaatsen")
	public ResponseEntity<List<String>> getStandplaatsPlaatsen(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN)
			|| clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN))
		{
			MammaAfspraakWijzigenFilterDto plaatsFilter = new MammaAfspraakWijzigenFilterDto();
			plaatsFilter.setClient(client);
			plaatsFilter.setBuitenRegio(false);
			plaatsFilter.setVanaf(currentDateSupplier.getLocalDate());
			plaatsFilter.setTotEnMet(currentDateSupplier.getLocalDate().plusYears(2));

			long start = System.currentTimeMillis();
			try
			{
				return ResponseEntity.ok(standplaatsService.getStandplaatsPlaatsenVanActievePeriodes(plaatsFilter, false));
			}
			finally
			{
				LOG.debug("getStandplaatsPlaatsen duurde: " + (System.currentTimeMillis() - start) + "ms");
			}
		}
		return createForbiddenResponse();
	}

	@PostMapping("/beschikbaarheid/plaats")
	public ResponseEntity<List<LocalDate>> getDagenMetBeschikbaarheidViaPlaats(Authentication authentication, @RequestBody MammaBeschikbaarheidPlaatsOpvragenDto plaatsOpvragenDto)
	{
		long start = System.currentTimeMillis();
		try
		{
			return getResponseMetBeschikbareDagen(authentication, plaatsOpvragenDto.getPlaats(), null);
		}
		finally
		{
			LOG.debug("getDagenMetBeschikbaarheidViaPlaats duurde: " + (System.currentTimeMillis() - start) + "ms");
		}
	}

	@GetMapping("/beschikbaarheid/afstand/{afstand}")
	public ResponseEntity<List<LocalDate>> getDagenMetBeschikbaarheidViaAfstand(Authentication authentication, @PathVariable String afstand)
	{
		long start = System.currentTimeMillis();
		try
		{
			return getResponseMetBeschikbareDagen(authentication, null, afstand);
		}
		finally
		{
			LOG.debug("getDagenMetBeschikbaarheidViaAfstand duurde: " + (System.currentTimeMillis() - start) + "ms");
		}
	}

	private ResponseEntity<List<LocalDate>> getResponseMetBeschikbareDagen(Authentication authentication, String plaats, String afstand)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN)
			|| clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN))
		{
			return ResponseEntity.ok().body(afspraakService.getAlleDatumsMetBeschikbareAfspraken(client, plaats, afstand));
		}
		return createForbiddenResponse();
	}

	@PostMapping("/zoeken")
	public ResponseEntity<List<MammaAfspraakOptieDto>> zoekAfspraak(Authentication authentication, @RequestBody MammaAfspraakZoekFilterDto body)
	{
		if (datumValidatieService.datumIsInHetVerleden(body.getVanaf()))
		{
			LOG.error("De gekozen datum ligt in het verleden");
			return ResponseEntity.badRequest().build();
		}

		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN)
			|| clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN))
		{
			MammaAfspraakWijzigenFilterDto filter = afspraakService.toAfspraakFilter(body, client, false);

			long start = System.currentTimeMillis();
			try
			{
				return ResponseEntity.ok()
					.body(baseAfspraakService.getKandidaatAfspraken(client, filter).stream().distinct()
						.map(afspraakDto -> afspraakService.toMammaKandidaatOptie(afspraakDto, client))
						.sorted(Comparator.comparing(MammaAfspraakOptieDto::getDatumTijd)).collect(Collectors.toList()));
			}
			finally
			{
				LOG.debug("zoekAfspraak duurde: " + (System.currentTimeMillis() - start) + "ms");
			}
		}
		return createForbiddenResponse();
	}

	@PostMapping("/maak")

	public ResponseEntity<String> maakAfspraak(Authentication authentication, @RequestBody MammaAfspraakOptieDto afspraakOptie)
	{
		Client client = getClient(authentication, hibernateService);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN)
			|| clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN))
		{
			ClientContactActie actie = new ClientContactActie();
			actie.setType(ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN);
			ClientContact contact = new ClientContact();
			contact.getActies().add(actie);
			contact.setClient(client);

			var opslaanObjecten = maakExtraOpslaanObjectenMakenAfspraak(afspraakOptie, client);

			var laatstMogelijkeAfspraakDatum = baseAfspraakService.laatstMogelijkeAfspraakDatum(client.getMammaDossier());
			if (laatstMogelijkeAfspraakDatum == null || !afspraakOptie.getDatumTijd().toLocalDate().isAfter(laatstMogelijkeAfspraakDatum))
			{
				try
				{
					clientContactService.mammaAfspraakMakenWijzigen(actie, client, opslaanObjecten, null, false, false);
					return ResponseEntity.ok().body(((Long) opslaanObjecten.get(ExtraOpslaanKey.MAMMA_NIEUWE_AFSPRAAK_ID)).toString());
				}
				catch (MammaTijdNietBeschikbaarException e)
				{
					return ResponseEntity.status(HttpStatus.CONFLICT).body("tijd.niet.beschikbaar");
				}
				catch (RuntimeException e)
				{
					LOG.error(e.getMessage(), e);
					return ResponseEntity.badRequest().body("afspraak.wijzigen.niet.mogelijk");
				}
			}
		}
		return createForbiddenResponse();
	}

	@NotNull
	private Map<ExtraOpslaanKey, Object> maakExtraOpslaanObjectenMakenAfspraak(MammaAfspraakOptieDto afspraakOptie, Client client)
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = new EnumMap<>(ExtraOpslaanKey.class);

		opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK, afspraakService.toAfspraak(afspraakOptie, client));
		opslaanObjecten.put(ExtraOpslaanKey.MAMMA_AFSPRAAK_FILTER, afspraakService.toAfspraakFilter(afspraakOptie.getFilter(), client, false));
		if (baseAfspraakService.briefKanNogVerzondenWorden(DateUtil.toUtilDate(afspraakOptie.getDatumTijd())))
		{
			opslaanObjecten.put(ExtraOpslaanKey.BEVESTIGINGS_TYPE, BevestigingsType.BRIEF);
		}
		return opslaanObjecten;
	}

	@PostMapping("/bevestiging")
	public ResponseEntity<String> maakAfspraakBevestiging(Authentication authentication, @RequestBody MammaAfspraakBevestigingDto afspraakBevestiging)
	{
		Client client = getClient(authentication, hibernateService);
		try
		{
			var opslaanObjecten = maakAfspraakBevestigingOpslaanObjecten(afspraakBevestiging, client);
			clientContactService.mammaAfspraakBevestigingMakenVanuitClientPortaal(client, opslaanObjecten);
			return ResponseEntity.ok().build();
		}
		catch (RuntimeException rte)
		{
			LOG.error(rte.getMessage(), rte);
			return ResponseEntity.badRequest().body("afspraak.bevestiging.niet.mogelijk");
		}
	}

	private Map<ExtraOpslaanKey, Object> maakAfspraakBevestigingOpslaanObjecten(MammaAfspraakBevestigingDto afspraakBevestiging, Client client)
	{
		var opslaanObjecten = new EnumMap<>(ExtraOpslaanKey.class);
		var laatsteAfspraakClient = MammaScreeningRondeUtil.getLaatsteAfspraak(client.getMammaDossier().getLaatsteScreeningRonde());
		if (laatsteAfspraakClient != null && afspraakBevestiging.getAfspraakId().equals(laatsteAfspraakClient.getId()))
		{
			opslaanObjecten.put(ExtraOpslaanKey.MAMMA_NIEUWE_AFSPRAAK_ID, afspraakBevestiging.getAfspraakId());
		}
		else
		{
			throw new IllegalStateException("Afspraak ID matcht niet met laatste afspraak van laatste ronde client");
		}

		if (BevestigingsType.BRIEF == afspraakBevestiging.getBevestigingsType() && baseAfspraakService.briefKanNogVerzondenWorden(laatsteAfspraakClient.getVanaf()))
		{
			opslaanObjecten.put(ExtraOpslaanKey.BEVESTIGINGS_TYPE, BevestigingsType.BRIEF);
		}
		else if (BevestigingsType.MAIL == afspraakBevestiging.getBevestigingsType())
		{
			valideerMail(afspraakBevestiging.getClientNieuwEmailAdres());
			opslaanObjecten.put(ExtraOpslaanKey.BEVESTIGINGS_TYPE, BevestigingsType.MAIL);
			opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_BEVESTIGING_MAIL_ADRES, afspraakBevestiging.getClientNieuwEmailAdres());
		}
		else
		{
			opslaanObjecten.put(ExtraOpslaanKey.BEVESTIGINGS_TYPE, BevestigingsType.GEEN);
		}

		if (afspraakBevestiging.isWilHerinneringsSms())
		{
			valideerMobielNummer(afspraakBevestiging.getClientNieuwMobielNummer());
			opslaanObjecten.put(ExtraOpslaanKey.SMS_STATUS, SmsStatus.TE_VERSTUREN);
			opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_HERINNERING_TELEFOONNUMMER, afspraakBevestiging.getClientNieuwMobielNummer());
		}
		else
		{
			opslaanObjecten.put(ExtraOpslaanKey.SMS_STATUS, SmsStatus.GEEN);
		}
		return opslaanObjecten;
	}

	private void valideerMail(String mailAdres)
	{
		if (!EmailUtil.isCorrectEmailadres(mailAdres))
		{
			throw new IllegalStateException("Emailadres is niet correct gevalideerd");
		}
	}

	private void valideerMobielNummer(String mobielNummer)
	{
		if (!TelefoonnummerUtil.isCorrectNederlandsMobielNummer(mobielNummer))
		{
			throw new IllegalStateException("Mobiel nummer is niet correct gevalideerd");
		}
	}

	@GetMapping("/huidige")
	public ResponseEntity<MammaHuidigeAfspraakDto> getHuidigeAfspraak(Authentication authentication)
	{
		Client client = getClient(authentication, hibernateService);
		MammaAfspraak huidigeAfspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(client.getMammaDossier().getLaatsteScreeningRonde());

		if (huidigeAfspraak != null && huidigeAfspraak.getStatus().equals(MammaAfspraakStatus.GEPLAND) && huidigeAfspraak.getVanaf().compareTo(currentDateSupplier.getDate()) >= 0)
		{
			return ResponseEntity.ok(afspraakService.toHuidigeAfspraakDto(huidigeAfspraak));
		}
		return ResponseEntity.ok().build();
	}
}
