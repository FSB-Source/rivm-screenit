package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakReserveringView;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;
import nl.rivm.screenit.repository.mamma.MammaAfspraakReserveringRepository;
import nl.rivm.screenit.repository.mamma.MammaCapaciteitBlokRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestClientException;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.mamma.MammaCapaciteitBlokSpecification.voorScreeningsEenheidInPeriode;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@Slf4j
public class MammaBaseCapaciteitsBlokServiceImpl implements MammaBaseCapaciteitsBlokService
{
	@Autowired
	private MammaCapaciteitBlokRepository capaciteitBlokRepository;

	@Autowired(required = false)
	private MammaBaseConceptPlanningsApplicatie conceptPlanningsApplicatie;

	@Autowired
	private MammaBaseAfspraakService afspraakService;

	@Autowired
	private MammaAfspraakReserveringRepository afspraakReserveringRepository;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public String saveOrUpdate(PlanningCapaciteitBlokDto blok, InstellingGebruiker ingelogdeGebruiker)
	{
		if (MammaCapaciteitBlokType.GEEN_SCREENING.equals(blok.blokType))
		{
			blok.aantalOnderzoeken = 0;
			blok.minderValideAfspraakMogelijk = false;
		}

		boolean isNieuw = blok.conceptId == null;
		try
		{
			conceptPlanningsApplicatie.sendCapaciteitBlok(blok, isNieuw, ingelogdeGebruiker);
		}
		catch (HttpClientErrorException | HttpServerErrorException se)
		{
			var responseBody = se.getResponseBodyAsString();
			LOG.error("Er ging iets mis bij het opslaan van de capaciteitsblok", se);
			return responseBody;
		}
		catch (RestClientException e)
		{
			return e.getMessage();
		}
		return null;
	}

	@Override
	public String delete(PlanningCapaciteitBlokDto blok, InstellingGebruiker loggedInInstellingGebruiker)
	{
		return conceptPlanningsApplicatie.deleteCapaciteitBlok(blok, loggedInInstellingGebruiker);
	}

	@Override
	public int getAantalAfsprakenOpBlok(PlanningCapaciteitBlokDto blokDto, boolean toDelete)
	{
		return conceptPlanningsApplicatie.getAantalAfsprakenOpBlok(blokDto, toDelete);
	}

	@Override
	public List<MammaCapaciteitBlok> getCapaciteitsBlokken(MammaScreeningsEenheid screeningsEenheid, Date vanafMoment, Date totEnMetMoment, boolean bepaalCapaciteit,
		Collection<MammaCapaciteitBlokType> blokTypes)
	{
		var zoekbereik = Range.closed(vanafMoment, totEnMetMoment);
		var capaciteitsBlokken = capaciteitBlokRepository.findAll(voorScreeningsEenheidInPeriode(screeningsEenheid, blokTypes, zoekbereik), Sort.by(MammaCapaciteitBlok_.VANAF));

		if (bepaalCapaciteit)
		{
			bepaalCapaciteit(capaciteitsBlokken, screeningsEenheid);
		}

		return capaciteitsBlokken;
	}

	private void bepaalCapaciteit(List<MammaCapaciteitBlok> capaciteitsBlokken, MammaScreeningsEenheid screeningsEenheid)
	{
		ScreeningOrganisatie screeningOrganisatie = (ScreeningOrganisatie) HibernateHelper.deproxy(screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio());
		for (MammaCapaciteitBlok capaciteitBlok : capaciteitsBlokken)
		{
			BigDecimal aantalOnderzoeken = new BigDecimal(capaciteitBlok.getAantalOnderzoeken());
			capaciteitBlok.setBeschikbareCapaciteit(aantalOnderzoeken.multiply(capaciteitBlok.getBlokType().getFactorType().getFactor(screeningOrganisatie)));

			afspraakService.bepaalBenodigdeCapaciteit(capaciteitBlok.getAfspraken(), screeningsEenheid);
			BigDecimal benodigdeCapaciteit = afspraakService.getBenodigdeCapaciteit(capaciteitBlok.getAfspraken());
			capaciteitBlok.setVrijeCapaciteit(capaciteitBlok.getBeschikbareCapaciteit().subtract(benodigdeCapaciteit));
		}
	}

	@Override
	public Collection<MammaCapaciteitBlokDto> getNietGeblokkeerdeCapaciteitsBlokDtos(MammaStandplaatsPeriode standplaatsPeriode, Date vanaf, Date totEnMet,
		Collection<MammaCapaciteitBlokType> blokTypes, Client client)
	{
		if (DateUtil.compareAfter(vanaf, totEnMet))
		{

			return Collections.emptyList();
		}
		var zoekBereik = Range.closed(vanaf, totEnMet);
		var screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
		var screeningOrganisatie = (ScreeningOrganisatie) Hibernate.unproxy(screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio());
		var standplaats = standplaatsPeriodeOverlaptMetZoekBereik(standplaatsPeriode, vanaf, totEnMet) ? standplaatsPeriode.getStandplaatsRonde().getStandplaats() : null;

		var capaciteitBlokProjecties = capaciteitBlokRepository.findNietGeblokkeerdeCapaciteitBlokken(zoekBereik, blokTypes, screeningsEenheid,
			screeningOrganisatie, standplaats);

		Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap = new HashMap<>();
		for (var projectie : capaciteitBlokProjecties)
		{
			var capaciteitBlokId = projectie.getBlokId();
			var afspraakVanaf = DateUtil.toLocalDateTime(projectie.getAfspraakVanaf());
			var capaciteitBlokDto = capaciteitBlokDtoMap.get(capaciteitBlokId);
			if (capaciteitBlokDto == null)
			{
				capaciteitBlokDto = new MammaCapaciteitBlokDto();
				capaciteitBlokDto.id = capaciteitBlokId;
				capaciteitBlokDto.vanaf = DateUtil.toLocalDateTime(projectie.getBlokVanaf());
				capaciteitBlokDto.tot = DateUtil.toLocalTime((projectie.getBlokTot()));
				capaciteitBlokDto.blokType = projectie.getBlokType();
				capaciteitBlokDto.aantalOnderzoeken = projectie.getAantalOnderzoeken();
				capaciteitBlokDto.minderValideAfspraakMogelijk = projectie.isMinderValideAfspraakMogelijk();
				var aantalOnderzoeken = new BigDecimal(capaciteitBlokDto.aantalOnderzoeken);
				capaciteitBlokDto.beschikbareCapaciteit = aantalOnderzoeken.multiply(capaciteitBlokDto.blokType.getFactorType().getFactor(screeningOrganisatie));
				capaciteitBlokDto.standplaatsPeriode = standplaatsPeriode;
				capaciteitBlokDtoMap.put(capaciteitBlokId, capaciteitBlokDto);
			}
			if (afspraakVanaf != null)
			{
				var doelgroep = projectie.getDoelgroep();
				var tehuisId = projectie.getTehuisId();
				var eersteOnderzoek = projectie.getEersteOnderzoek();
				var opkomstkans = projectie.getOpkomstkans();
				var factor = MammaFactorType.getFactorType(tehuisId != null, doelgroep, eersteOnderzoek).getFactor(screeningOrganisatie);
				var afspraakDto = new MammaAfspraakDto();
				afspraakDto.setCapaciteitBlokDto(capaciteitBlokDto);
				afspraakDto.setVanaf(afspraakVanaf);
				afspraakDto.setBenodigdeCapaciteit(factor.multiply(opkomstkans));
				afspraakDto.setMinderValide(doelgroep.equals(MammaDoelgroep.MINDER_VALIDE));
				afspraakDto.setDubbeleTijd(doelgroep.equals(MammaDoelgroep.DUBBELE_TIJD));
				capaciteitBlokDto.afspraakDtos.add(afspraakDto);
			}
			var benodigdeCapaciteit = capaciteitBlokDto.afspraakDtos.stream().map(MammaAfspraakDto::getBenodigdeCapaciteit).reduce(BigDecimal.ZERO,
				BigDecimal::add);
			capaciteitBlokDto.vrijeCapaciteit = capaciteitBlokDto.beschikbareCapaciteit.subtract(benodigdeCapaciteit);
		}
		haalAfspraakReserveringenOpEnVoegToeAanCapaciteitBlokken(capaciteitBlokDtoMap, screeningOrganisatie, client);
		sorteerCapaciteitBlokkenOpAfspraakTijdEnZetAfspraakTot(capaciteitBlokDtoMap);
		return capaciteitBlokDtoMap.values();
	}

	private boolean standplaatsPeriodeOverlaptMetZoekBereik(MammaStandplaatsPeriode standplaatsPeriode, Date zoekVanaf, Date zoekTotEnMet)
	{
		return (standplaatsPeriode.getVanaf().before(zoekTotEnMet) || standplaatsPeriode.getVanaf().equals(zoekTotEnMet))
			&& (standplaatsPeriode.getTotEnMet().after(zoekVanaf) || standplaatsPeriode.getTotEnMet().equals(zoekVanaf));
	}

	private void haalAfspraakReserveringenOpEnVoegToeAanCapaciteitBlokken(Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap, ScreeningOrganisatie screeningOrganisatie,
		Client client)
	{
		var maximaleReserveringsTijd = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_RESERVERING_GELDIG_VOOR.name(), 0);
		var vroegstOpTeHalenReservering = currentDateSupplier.getLocalDateTime().minusMinutes(maximaleReserveringsTijd);

		var capaciteitblokIds = capaciteitBlokDtoMap.keySet();
		if (!capaciteitblokIds.isEmpty())
		{
			var clientId = client != null ? client.getId() : null;
			var reserveringDtos = afspraakReserveringRepository.haalReserveringenOpVoorCapaciteitsblokken(vroegstOpTeHalenReservering, capaciteitblokIds, clientId);
			reserveringDtos.forEach(reservering -> converteerReserveringNaarAfspraakInCapaciteitBlok(reservering, capaciteitBlokDtoMap, screeningOrganisatie));
		}
	}

	private void converteerReserveringNaarAfspraakInCapaciteitBlok(MammaAfspraakReserveringView reservering, Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap,
		ScreeningOrganisatie screeningOrganisatie)
	{
		var doelgroep = reservering.getDoelgroep();
		var factor = MammaFactorType.getFactorType(reservering.getTehuisId() != null, doelgroep, reservering.getEersteOnderzoek()).getFactor(screeningOrganisatie);

		var capaciteitBlokDto = capaciteitBlokDtoMap.get(reservering.getCapaciteitBlokId());
		var afspraakDto = new MammaAfspraakDto();
		afspraakDto.setCapaciteitBlokDto(capaciteitBlokDto);
		afspraakDto.setVanaf(reservering.getVanaf());
		afspraakDto.setBenodigdeCapaciteit(factor.multiply(reservering.getOpkomstkans()));
		afspraakDto.setMinderValide(doelgroep.equals(MammaDoelgroep.MINDER_VALIDE));
		afspraakDto.setDubbeleTijd(doelgroep.equals(MammaDoelgroep.DUBBELE_TIJD));
		capaciteitBlokDto.afspraakDtos.add(afspraakDto);
	}

	private void sorteerCapaciteitBlokkenOpAfspraakTijdEnZetAfspraakTot(Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap)
	{
		capaciteitBlokDtoMap.values().forEach(capaciteitBlokDto ->
		{
			capaciteitBlokDto.afspraakDtos.sort(Comparator.comparing(MammaAfspraakDto::getVanaf));
			MammaAfspraakDto vorigeAfspraakDto = null;
			for (var afspraakDto : capaciteitBlokDto.afspraakDtos)
			{
				if (vorigeAfspraakDto != null)
				{
					vorigeAfspraakDto.setTot(afspraakDto.getVanaf().toLocalTime());
				}
				vorigeAfspraakDto = afspraakDto;
			}
			if (vorigeAfspraakDto != null)
			{
				vorigeAfspraakDto.setTot(capaciteitBlokDto.tot);
			}
		});
	}

	@Override
	public MammaCapaciteit getCapaciteit(Collection<MammaCapaciteitBlokDto> capaciteitBlokDtos)
	{
		MammaCapaciteit capaciteit = new MammaCapaciteit();
		HashMap<MammaCapaciteit.BlokTypeCapaciteit, HashMap<String, BigDecimal>> blokTypeVrijeCapaciteitPerDag = new HashMap<>();
		for (MammaCapaciteitBlokDto capaciteitBlokDto : capaciteitBlokDtos)
		{
			MammaCapaciteit.BlokTypeCapaciteit blokTypeCapaciteit = capaciteit.capaciteitMap.get(capaciteitBlokDto.blokType);
			blokTypeCapaciteit.beschikbareCapaciteit = blokTypeCapaciteit.beschikbareCapaciteit.add(capaciteitBlokDto.beschikbareCapaciteit);
			blokTypeCapaciteit.vrijeCapaciteit = blokTypeCapaciteit.vrijeCapaciteit.add(capaciteitBlokDto.vrijeCapaciteit);

			if (!blokTypeVrijeCapaciteitPerDag.containsKey(blokTypeCapaciteit))
			{
				blokTypeVrijeCapaciteitPerDag.put(blokTypeCapaciteit, new HashMap<>());
			}

			String standplaatsPeriodeDag = capaciteitBlokDto.vanaf.toLocalDate().format(DateUtil.LOCAL_DATE_FORMAT) + "-" + capaciteitBlokDto.standplaatsPeriode;
			BigDecimal vrijeCapaciteitStandplaatsPeriodeDag = BigDecimal.ZERO;

			HashMap<String, BigDecimal> vrijeCapaciteitPerDag = blokTypeVrijeCapaciteitPerDag.get(blokTypeCapaciteit);
			if (vrijeCapaciteitPerDag.containsKey(standplaatsPeriodeDag))
			{
				vrijeCapaciteitStandplaatsPeriodeDag = vrijeCapaciteitPerDag.get(standplaatsPeriodeDag);
			}
			vrijeCapaciteitStandplaatsPeriodeDag = vrijeCapaciteitStandplaatsPeriodeDag.add(capaciteitBlokDto.vrijeCapaciteit);
			vrijeCapaciteitPerDag.put(standplaatsPeriodeDag, vrijeCapaciteitStandplaatsPeriodeDag);
		}

		for (MammaCapaciteit.BlokTypeCapaciteit blokTypeCapaciteit : capaciteit.capaciteitMap.values())
		{
			if (blokTypeVrijeCapaciteitPerDag.containsKey(blokTypeCapaciteit))
			{
				for (BigDecimal vrijeDagCapaciteit : blokTypeVrijeCapaciteitPerDag.get(blokTypeCapaciteit).values())
				{
					if (vrijeDagCapaciteit.compareTo(BigDecimal.ZERO) < 0)
					{
						blokTypeCapaciteit.negatieveVrijeCapaciteit = blokTypeCapaciteit.negatieveVrijeCapaciteit.add(vrijeDagCapaciteit.multiply(BigDecimal.valueOf(-1)));
					}
				}
			}
			blokTypeCapaciteit.benutteCapaciteit = blokTypeCapaciteit.beschikbareCapaciteit.subtract(blokTypeCapaciteit.vrijeCapaciteit);
		}
		return capaciteit;
	}

	@Override
	public MammaCapaciteitBlok getCapaciteitsBlokOpTijdstipVoorSe(Client client, MammaScreeningsEenheid screeningsEenheid, Date nu)
	{

		List<MammaCapaciteitBlok> capaciteitBlokkenNu = getCapaciteitsBlokken(screeningsEenheid, nu, nu, false,
			Arrays.asList(MammaCapaciteitBlokType.TEHUIS, MammaCapaciteitBlokType.REGULIER));
		if (client.getMammaDossier().getTehuis() != null)
		{
			return getMammaCapaciteitBlokVoorType(capaciteitBlokkenNu, Collections.singletonList(MammaCapaciteitBlokType.TEHUIS));
		}
		else
		{
			return getMammaCapaciteitBlokVoorType(capaciteitBlokkenNu, Collections.singletonList(MammaCapaciteitBlokType.REGULIER));
		}
	}

	private MammaCapaciteitBlok getMammaCapaciteitBlokVoorType(List<MammaCapaciteitBlok> capaciteitBlokkenNu, List<MammaCapaciteitBlokType> types)
	{
		if (!capaciteitBlokkenNu.isEmpty())
		{
			Optional<MammaCapaciteitBlok> mogelijkCapaciteitsBlok = capaciteitBlokkenNu.stream().filter(mammaCapaciteitBlok -> types.contains(mammaCapaciteitBlok.getBlokType()))
				.findAny();
			if (mogelijkCapaciteitsBlok.isPresent())
			{
				return mogelijkCapaciteitsBlok.get();
			}
		}
		return null;
	}

}
