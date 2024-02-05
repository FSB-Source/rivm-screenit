package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.dao.mamma.MammaBaseBlokkadeDao;
import nl.rivm.screenit.dao.mamma.MammaBaseStandplaatsPeriodeDao;
import nl.rivm.screenit.mamma.se.dao.MammaScreeningsEenheidDao;
import nl.rivm.screenit.mamma.se.dto.DagAfsluitingDto;
import nl.rivm.screenit.mamma.se.dto.DagPlanningSamenvattingDto;
import nl.rivm.screenit.mamma.se.dto.DagProductieDto;
import nl.rivm.screenit.mamma.se.dto.DagSynchronisatieDto;
import nl.rivm.screenit.mamma.se.repository.MammaAfspraakRepository;
import nl.rivm.screenit.mamma.se.service.DagverslagService;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidService;
import nl.rivm.screenit.mamma.se.service.OnderzoekService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EnvironmentUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
@RequiredArgsConstructor
public class DagverslagServiceImpl implements DagverslagService
{
	private final HibernateService hibernateService;

	private final MammaAfspraakService mammaAfspraakService;

	private final OnderzoekService mammaOnderzoekService;

	private final MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	private final MammaScreeningsEenheidDao screeningsEenheidDao;

	private final MammaBaseBlokkadeDao baseBlokkadeDao;

	private final MammaBaseStandplaatsPeriodeDao baseStandplaatsPeriodeDao;

	private final ICurrentDateSupplier currentDateSupplier;

	private final MammaAfspraakRepository afspraakRepository;

	private final MammaScreeningsEenheidService screeningsEenheidService;

	@Override
	public Map<String, DagProductieDto> getDagproductieVanSeMedewerkers(String seCode, Date datum)
	{
		Map<String, DagProductieDto> result = new HashMap<>();

		mammaAfspraakService.getIngeschrevenByGebruikerOpDatumVoorSe(datum, seCode).entrySet().stream()
			.filter(entry -> entry.getKey() != null)
			.forEach(entry -> getOrCreateDagproductieDto(entry.getKey(), result).setIngeschrevenCount(entry.getValue()));

		mammaOnderzoekService.getOnderzochtByGebruikerOpDatumVoorSe(datum, seCode).entrySet().stream()
			.filter(entry -> entry.getKey() != null)
			.forEach(entry -> getOrCreateDagproductieDto(entry.getKey(), result).setOnderzochtCount(entry.getValue()));

		mammaOnderzoekService.getAfgerondByGebruikerOpDatumVoorSe(datum, seCode).entrySet().stream()
			.filter(entry -> entry.getKey() != null)
			.forEach(entry -> getOrCreateDagproductieDto(entry.getKey(), result).setAfgerondCount(entry.getValue()));

		mammaOnderzoekService.getOnderbrokenByGebruikerOpDatumVoorSe(datum, seCode).entrySet().stream()
			.filter(entry -> entry.getKey() != null)
			.forEach(entry -> getOrCreateDagproductieDto(entry.getKey(), result).setOnderbrokenCount(entry.getValue()));

		mammaOnderzoekService.getOnvolledigByGebruikerOpDatumVoorSe(datum, seCode).entrySet().stream()
			.filter(entry -> entry.getKey() != null)
			.forEach(entry -> getOrCreateDagproductieDto(entry.getKey(), result).setOnvolledigCount(entry.getValue()));

		mammaOnderzoekService.getAfwijkingenByGebruikerOpDatumVoorSe(datum, seCode).entrySet().stream()
			.filter(entry -> entry.getKey() != null)
			.forEach(entry -> getOrCreateDagproductieDto(entry.getKey(), result).setAfwijkingenCount(entry.getValue()));
		return result;
	}

	private DagProductieDto getOrCreateDagproductieDto(Long instellingGebruikerId, Map<String, DagProductieDto> result)
	{
		var dagproductieDto = result.get(getDisplayName(instellingGebruikerId));
		if (dagproductieDto == null)
		{
			dagproductieDto = new DagProductieDto();
			result.put(getDisplayName(instellingGebruikerId), dagproductieDto);
		}
		return dagproductieDto;
	}

	private String getDisplayName(long instellingGebruikerId)
	{
		var instellingGebruiker = hibernateService.get(InstellingGebruiker.class, instellingGebruikerId);
		return NaamUtil.getNaamGebruiker(instellingGebruiker.getMedewerker());
	}

	@Override
	public DagAfsluitingDto getDoorgevoerdCountVanDag(String seCode, Date datum)
	{
		var dagAfsluitingDto = new DagAfsluitingDto();
		dagAfsluitingDto.setAantalDoorgevoerd(mammaOnderzoekService.getAantalDoorgevoerdVanDag(datum, seCode));
		return dagAfsluitingDto;
	}

	@Override
	public DagSynchronisatieDto getSynchronisatieCountVanDag(String seCode, Date datum)
	{
		var synchronisatieDto = new DagSynchronisatieDto();
		synchronisatieDto.setGemaakt(mammaOnderzoekService.getAantalOnderzoekenMetBeelden(datum, seCode));
		synchronisatieDto.setVerwerkt(mammaOnderzoekService.getAantalOnderzoekenMetBeeldenBeschikbaarInIms(datum, seCode));
		return synchronisatieDto;
	}

	@Override
	public LocalDate getDatumVanOudsteNietAfgeslotenOnderzoek(String seCode)
	{
		return mammaAfspraakService.getDatumVanOudsteNietAfgeslotenOnderzoek(seCode);
	}

	@Override
	public DagPlanningSamenvattingDto getPlanningSamenvattingVanDeDag(String seCode, Date datum)
	{
		var screeningsEenheid = screeningsEenheidDao.getActieveScreeningsEenheidByCode(seCode);
		var standplaatsPeriode = baseStandplaatsPeriodeDao.getStandplaatsPeriode(screeningsEenheid, datum);

		if (standplaatsPeriode == null)
		{
			return new DagPlanningSamenvattingDto();
		}
		var standplaats = standplaatsPeriode.getStandplaatsRonde().getStandplaats();
		var actieveBlokkades = baseBlokkadeDao.getActieveBlokkadesVoorSE(standplaats, screeningsEenheid, datum);

		if (!actieveBlokkades.isEmpty() || (!EnvironmentUtil.getBooleanEnvironmentVariable("SE_DAGVERSLAG_INCL_CAPACITEIT", true) && DateUtil.isZelfdeDag(
			currentDateSupplier.getLocalDate(), datum)))
		{
			return new DagPlanningSamenvattingDto();
		}
		else
		{
			var capaciteitsBlokken = baseCapaciteitsBlokService.getCapaciteitsBlokken(screeningsEenheid, datum,
				DateUtil.toUtilDate(DateUtil.toLocalDate(datum).plusDays(1)), true, Arrays.asList(MammaCapaciteitBlokType.REGULIER, MammaCapaciteitBlokType.TEHUIS));

			return maakEnVulDagPlanningSamenvattingDto(capaciteitsBlokken, seCode, datum);
		}
	}

	private DagPlanningSamenvattingDto maakEnVulDagPlanningSamenvattingDto(List<MammaCapaciteitBlok> capaciteitsBlokken, String seCode, Date datum)
	{
		var vrijeCapaciteit = BigDecimal.ZERO;
		var beschikbareCapaciteit = BigDecimal.ZERO;
		for (MammaCapaciteitBlok blok : capaciteitsBlokken)
		{
			vrijeCapaciteit = vrijeCapaciteit.add(blok.getVrijeCapaciteit());
			beschikbareCapaciteit = beschikbareCapaciteit.add(blok.getBeschikbareCapaciteit());
		}
		LocalDateTime starttijd = DateUtil.toLocalDateTime(capaciteitsBlokken.stream().map(MammaCapaciteitBlok::getVanaf).min(Comparator.naturalOrder()).orElse(null));
		LocalDateTime eindtijd = DateUtil.toLocalDateTime(capaciteitsBlokken.stream().map(MammaCapaciteitBlok::getTot).max(Comparator.naturalOrder()).orElse(null));

		var statistiekenDto = new DagPlanningSamenvattingDto();
		statistiekenDto.setDagCapaciteit(beschikbareCapaciteit);
		statistiekenDto.setBeschikbaarheid(vrijeCapaciteit.setScale(1, RoundingMode.HALF_UP));
		statistiekenDto.setStarttijd(starttijd);
		statistiekenDto.setEindtijd(eindtijd);

		if (!screeningsEenheidService.magSeDaglijstInzienVanDatum(seCode, DateUtil.toLocalDate(datum)))
		{
			zetAfspraakStatussen(seCode, datum, statistiekenDto);
		}

		return statistiekenDto;
	}

	private void zetAfspraakStatussen(String seCode, Date datum, DagPlanningSamenvattingDto statistiekenDto)
	{
		var afspraakStatussen = new MammaAfspraakStatus[] { MammaAfspraakStatus.GEPLAND, MammaAfspraakStatus.BEEINDIGD };
		var aantalAfsprakenPerStatus = afspraakRepository.getAantalAfsprakenPerStatus(seCode, datum, DateUtil.eindDag(datum),
			afspraakStatussen);
		statistiekenDto.setAantalVerwacht(aantalAfsprakenPerStatus.getAantalVerwacht());
		statistiekenDto.setAantalAfgerond(aantalAfsprakenPerStatus.getAantalAfgerond());
		statistiekenDto.setAantalOnderbroken(aantalAfsprakenPerStatus.getAantalOnderbroken());
		statistiekenDto.setAantalOnvolledig(aantalAfsprakenPerStatus.getAantalOnvolledig());
	}

}
