package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaScreeningsEenheidDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseKandidaatAfsprakenDeterminatiePeriode;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaCapaciteitZoeken;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaKandidaatAfspraak;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaRationaal;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.TimeRange;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
@Scope("prototype")
@Slf4j
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseKandidaatAfsprakenDeterminatiePeriodeImpl implements MammaBaseKandidaatAfsprakenDeterminatiePeriode
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaBaseCapaciteitsBlokService capaciteitsBlokService;

	@Autowired
	private MammaBaseDossierService dossierService;

	private static final BigDecimal TEN = new BigDecimal("10");

	private static final BigDecimal MINIMUM_OPKOMSTKANS = new BigDecimal("0.1");

	private boolean meerdereKandidaten = false;

	private List<MammaStandplaatsPeriode> standplaatsPeriodeList;

	private final Map<MammaStandplaatsPeriode, LocalDate> standplaatsPeriodeVanafMap = new HashMap<>();

	private final Map<MammaStandplaatsPeriode, LocalDate> standplaatsPeriodeTotEnMetMap = new HashMap<>();

	private MammaDossier dossier;

	private BigDecimal factor;

	private BigDecimal benodigdeCapaciteit;

	private boolean extraOpties = false;

	private LocalDate vandaagOfMorgen;

	private LocalDate aflopendVanaf;

	private MammaCapaciteit capaciteit;

	private MammaCapaciteitBlokType blokType;

	private Map<LocalDate, List<MammaCapaciteitBlokDto>> dateCapaciteitBlokMap = new TreeMap<>();

	private List<DeterminatieDag> determinatieDagList = new ArrayList<>();

	private MammaScreeningsEenheidDto screeningsEenheidDto;

	private int minimaleDagCapaciteitMinderValideAfspraken;

	private Long laatsteMinderValideAfspraakCapaciteitBlokId;

	private boolean isMindervalide;

	private boolean isDubbeleTijd;

	private void init(List<MammaStandplaatsPeriode> standplaatsPeriodeList, MammaDossier dossier, BigDecimal voorlopigeOpkomstkans,
		Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen)
	{
		this.standplaatsPeriodeList = standplaatsPeriodeList;
		this.dossier = dossier;
		isMindervalide = dossier.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE);
		isDubbeleTijd = dossier.getDoelgroep().equals(MammaDoelgroep.DUBBELE_TIJD);

		var standplaatsPeriode = standplaatsPeriodeList.get(0);

		var screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
		screeningsEenheidDto = MammaScreeningsEenheidDto.vanEntiteit(screeningsEenheid);

		var screeningOrganisatie = (ScreeningOrganisatie) HibernateHelper.deproxy(screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio());
		factor = dossierService.getFactorType(dossier).getFactor(screeningOrganisatie);
		minimaleDagCapaciteitMinderValideAfspraken = screeningOrganisatie.getMinimaleDagCapaciteitMinderValideAfspraken();

		benodigdeCapaciteit = factor.multiply(voorlopigeOpkomstkans.compareTo(MINIMUM_OPKOMSTKANS) >= 0 ? voorlopigeOpkomstkans : MINIMUM_OPKOMSTKANS);

		vandaagOfMorgen = getHuidigeDagVoorPlannenAfspraken();

		aflopendVanaf = DateUtil.plusWerkdagen(vandaagOfMorgen, capaciteitVolledigBenutTotEnMetAantalWerkdagen);

		if (isMindervalide)
		{
			var laatsteAfspraak = dossier.getLaatsteScreeningRonde() != null ? MammaScreeningRondeUtil.getLaatsteAfspraak(dossier.getLaatsteScreeningRonde()) : null;
			if (laatsteAfspraak != null && laatsteAfspraak.getCapaciteitBlok() != null)
			{
				laatsteMinderValideAfspraakCapaciteitBlokId = laatsteAfspraak.getCapaciteitBlok().getId();
			}
		}
	}

	@Override
	public LocalDate getHuidigeDagVoorPlannenAfspraken()
	{
		var nu = dateSupplier.getLocalDateTime();
		return nu.toLocalTime().isBefore(Constants.BK_EINDTIJD_DAG) ? nu.toLocalDate() : nu.toLocalDate().plusDays(1);
	}

	@Override
	public List<MammaKandidaatAfspraak> getKandidaatAfspraken(MammaDossier dossier, MammaStandplaatsPeriode standplaatsPeriode, LocalDate vanaf, LocalDate totEnMet,
		boolean extraOpties, BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen, boolean corrigeerNegatieveVrijeCapaciteit)
	{
		init(Collections.singletonList(standplaatsPeriode), dossier, voorlopigeOpkomstkans, capaciteitVolledigBenutTotEnMetAantalWerkdagen);
		this.extraOpties = extraOpties || isMindervalide;

		standplaatsPeriodeVanafMap.put(standplaatsPeriode, vanaf);
		standplaatsPeriodeTotEnMetMap.put(standplaatsPeriode, totEnMet);

		meerdereKandidaten = true;
		return getKandidaatAfspraken(corrigeerNegatieveVrijeCapaciteit, dossier.getClient());
	}

	@Override
	public MammaKandidaatAfspraak getKandidaatAfspraakBulkVerzetten(MammaDossier dossier, MammaStandplaatsPeriode standplaatsPeriode, LocalDate vanaf, LocalDate totEnMet,
		BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen)
	{
		return getKandidaatAfspraken(dossier, standplaatsPeriode, vanaf, totEnMet, false, voorlopigeOpkomstkans, capaciteitVolledigBenutTotEnMetAantalWerkdagen, false).get(0);
	}

	@Override
	public MammaKandidaatAfspraak getKandidaatAfspraakUitnodiging(MammaDossier dossier, MammaStandplaatsRonde standplaatsRonde, BigDecimal voorlopigeOpkomstkans,
		Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen, Integer afspraakBijUitnodigenVanafAantalWerkdagen)
		throws MammaOnvoldoendeVrijeCapaciteitException
	{
		var standplaatsPeriodes = standplaatsRonde.getStandplaatsPerioden().stream()
			.filter(standplaatsPeriode -> standplaatsPeriode.getScreeningsEenheid().getUitnodigenTotEnMet() != null).collect(Collectors.toList());
		init(standplaatsPeriodes, dossier, voorlopigeOpkomstkans, capaciteitVolledigBenutTotEnMetAantalWerkdagen);

		var uitnodigenVanafDatum = DateUtil.plusWerkdagen(vandaagOfMorgen, afspraakBijUitnodigenVanafAantalWerkdagen);
		for (var standplaatsPeriode : standplaatsPeriodes)
		{
			standplaatsPeriodeVanafMap.put(standplaatsPeriode, Collections.max(Arrays.asList(uitnodigenVanafDatum, DateUtil.toLocalDate(standplaatsPeriode.getVanaf()))));
			standplaatsPeriodeTotEnMetMap.put(standplaatsPeriode,
				DateUtil.toLocalDate(Collections.min(Arrays.asList(standplaatsPeriode.getScreeningsEenheid().getUitnodigenTotEnMet(), standplaatsPeriode.getTotEnMet()))));
		}

		var kandidaatAfspraken = getKandidaatAfspraken(false, dossier.getClient());
		if (kandidaatAfspraken.isEmpty())
		{
			throw new MammaOnvoldoendeVrijeCapaciteitException();
		}
		return kandidaatAfspraken.get(0);
	}

	private void bepaalDeterminatiePeriode(Client client)
	{
		var nietGeblokkeerdeCapaciteitsBlokken = new ArrayList<MammaCapaciteitBlokDto>();

		for (var standplaatsPeriode : standplaatsPeriodeList)
		{
			var vanafDate = DateUtil.toUtilDate(standplaatsPeriodeVanafMap.get(standplaatsPeriode));
			var totEnMetDate = DateUtil.toUtilDate(standplaatsPeriodeTotEnMetMap.get(standplaatsPeriode).atTime(Constants.BK_EINDTIJD_DAG));

			var standplaatsPeriodeNietGeblokkeerdeCapaciteitsBlokken = capaciteitsBlokService.getNietGeblokkeerdeCapaciteitsBlokDtos(
				standplaatsPeriode, vanafDate, totEnMetDate, blokType == null ? null : Collections.singletonList(blokType), client);

			nietGeblokkeerdeCapaciteitsBlokken.addAll(standplaatsPeriodeNietGeblokkeerdeCapaciteitsBlokken);
		}

		blokType = dossier.getTehuis() == null ? MammaCapaciteitBlokType.REGULIER : MammaCapaciteitBlokType.TEHUIS;

		capaciteit = capaciteitsBlokService.getCapaciteit(nietGeblokkeerdeCapaciteitsBlokken);

		var capaciteitsBlokMap = new TreeMap<LocalDate, List<MammaCapaciteitBlokDto>>();
		nietGeblokkeerdeCapaciteitsBlokken.stream()
			.filter(blok -> blok.blokType == blokType)
			.forEach(blok ->
			{
				var vanafDatum = blok.vanaf.toLocalDate();
				capaciteitsBlokMap.computeIfAbsent(vanafDatum, v -> new ArrayList<>()).add(blok);
			});

		capaciteitsBlokMap.forEach((key, value) -> value.sort(Comparator.comparing(b -> b.vanaf)));

		dateCapaciteitBlokMap = capaciteitsBlokMap;
		determinatieDagList = capaciteitsBlokMap.keySet().stream()
			.map(DeterminatieDag::new)
			.filter(determinatieDag -> determinatieDag.beschikbareCapaciteit.subtract(determinatieDag.benutteCapaciteit).compareTo(BigDecimal.ZERO) >= 0
				&& !determinatieDag.getDeterminatieBlokList().isEmpty())
			.collect(Collectors.toList());
	}

	private List<MammaKandidaatAfspraak> getKandidaatAfspraken(boolean corrigeerNegatieveVrijeCapaciteit, Client client)
	{
		bepaalDeterminatiePeriode(client);

		var totaleCapaciteit = capaciteit.getCapaciteit(blokType);

		var kandidaatAfspraken = new ArrayList<MammaKandidaatAfspraak>();
		if (isMindervalide && standplaatsPeriodeList.get(0).getStandplaatsRonde().getMinderValideUitwijkStandplaats() != null)
		{
			return kandidaatAfspraken;
		}

		var totaleVrijeCapaciteit = totaleCapaciteit.getVrijeCapaciteit(corrigeerNegatieveVrijeCapaciteit);

		if (meerdereKandidaten && determinatieDagList.isEmpty())
		{
			return kandidaatAfspraken;
		}

		var aantalKandidatenVrijeCapaciteit = totaleVrijeCapaciteit.divide(benodigdeCapaciteit, 5, RoundingMode.HALF_UP).setScale(0, RoundingMode.UP).intValue();
		var maxAantalKandidatenTonen = totaleCapaciteit.beschikbareCapaciteit.divide(TEN, 5, RoundingMode.HALF_UP).setScale(0, RoundingMode.UP).intValue();
		var maxZoekPogingen = aantalKandidatenVrijeCapaciteit + 1;

		LOG.debug("totaleVrijeCapaciteit: {}, benodigdeCapaciteit: {}, aantalKandidatenVrijeCapaciteit: {}, maxAantalKandidatenTonen zonder extraOpties: {}, extraOpties: {}",
			totaleVrijeCapaciteit, benodigdeCapaciteit, aantalKandidatenVrijeCapaciteit, maxAantalKandidatenTonen, extraOpties);

		for (var i = 0; i < maxZoekPogingen; i++)
		{
			if (aantalKandidatenVrijeCapaciteit == 0 || !extraOpties && maxAantalKandidatenTonen == 0
				|| !meerdereKandidaten && !kandidaatAfspraken.isEmpty())
			{
				break;
			}

			bepaalDoelCapaciteit(blokType, corrigeerNegatieveVrijeCapaciteit);
			var kandidaatAfspraak = MammaCapaciteitZoeken.elementMetRelatiefMeesteVrijeCapaciteit(determinatieDagList).getKandidaatAfspraak(benodigdeCapaciteit);
			aantalKandidatenVrijeCapaciteit--;
			if (kandidaatAfspraak.isGeldigeAfspraak())
			{
				maxAantalKandidatenTonen--;
				kandidaatAfspraken.add(kandidaatAfspraak);
			}
		}

		return kandidaatAfspraken;
	}

	private void bepaalDoelCapaciteit(MammaCapaciteitBlokType blokType, boolean corrigeerNegatieveVrijeCapaciteit)
	{
		BigDecimal aflopendPerDag;
		var blokTypeCapaciteit = capaciteit.getCapaciteit(blokType);
		if (blokTypeCapaciteit.getVrijeCapaciteit(corrigeerNegatieveVrijeCapaciteit).compareTo(BigDecimal.ZERO) <= 0)
		{
			aflopendPerDag = BigDecimal.ZERO;
		}
		else
		{
			var totaleCapaciteitInAflopendePeriode = BigDecimal.ZERO;
			var beschikbareCapaciteitInAflopendePeriode = BigDecimal.ZERO;
			var vrijeCapaciteitInAflopendePeriode = BigDecimal.ZERO;

			for (var determinatieDag : determinatieDagList)
			{
				if (!determinatieDag.datum.isBefore(aflopendVanaf))
				{
					totaleCapaciteitInAflopendePeriode = totaleCapaciteitInAflopendePeriode.add(determinatieDag.aflopendDag.multiply(determinatieDag.beschikbareCapaciteit));
					beschikbareCapaciteitInAflopendePeriode = beschikbareCapaciteitInAflopendePeriode.add(determinatieDag.beschikbareCapaciteit);
					vrijeCapaciteitInAflopendePeriode = vrijeCapaciteitInAflopendePeriode.add(determinatieDag.beschikbareCapaciteit.subtract(determinatieDag.benutteCapaciteit));
				}
			}

			if (beschikbareCapaciteitInAflopendePeriode.compareTo(BigDecimal.ZERO) == 0)
			{
				aflopendPerDag = BigDecimal.ONE;
			}
			else
			{
				var aflopendGemiddeldeDag = totaleCapaciteitInAflopendePeriode.divide(beschikbareCapaciteitInAflopendePeriode, 10, RoundingMode.HALF_UP);

				if (aflopendGemiddeldeDag.compareTo(BigDecimal.ZERO) != 0)
				{
					var aflopendGemiddeldeDoel = vrijeCapaciteitInAflopendePeriode.divide(beschikbareCapaciteitInAflopendePeriode, 10, RoundingMode.HALF_UP);

					aflopendPerDag = aflopendGemiddeldeDoel.divide(aflopendGemiddeldeDag, 10, RoundingMode.HALF_UP);
				}
				else
				{
					aflopendPerDag = BigDecimal.ONE;
				}
			}
		}
		determinatieDagList.forEach(determinatieDag -> determinatieDag.bepaalDoelCapaciteit(aflopendPerDag));
	}

	private class DeterminatieDag extends MammaRationaal
	{
		private final LocalDate datum;

		private BigDecimal beschikbareCapaciteit = BigDecimal.ZERO;

		private BigDecimal benutteCapaciteit = BigDecimal.ZERO;

		private BigDecimal doelCapaciteit = BigDecimal.ZERO;

		@Getter
		private final List<DeterminatieBlok> determinatieBlokList = new ArrayList<>();

		private final BigDecimal aflopendDag;

		private final boolean genoegDagCapaciteitVoorMinderValide;

		private DeterminatieDag(LocalDate datum)
		{
			this.datum = datum;
			aflopendDag = new BigDecimal(ChronoUnit.DAYS.between(aflopendVanaf, datum.plusDays(1L)));

			var totaleBeschikbareDagCapaciteit = new AtomicReference<BigDecimal>(BigDecimal.ZERO);

			dateCapaciteitBlokMap.get(datum)
				.forEach(capaciteitBlok ->
				{
					var determinatieBlok = new DeterminatieBlok(capaciteitBlok, datum);
					totaleBeschikbareDagCapaciteit.set(totaleBeschikbareDagCapaciteit.get().add(capaciteitBlok.beschikbareCapaciteit));
					if (!isMindervalide || !determinatieBlok.getMinderValidePeriodeList().isEmpty())
					{
						determinatieBlokList.add(determinatieBlok);

						beschikbareCapaciteit = beschikbareCapaciteit.add(determinatieBlok.beschikbareCapaciteit);
						benutteCapaciteit = benutteCapaciteit.add(determinatieBlok.benutteCapaciteit);
					}
				});

			genoegDagCapaciteitVoorMinderValide = totaleBeschikbareDagCapaciteit.get().compareTo(BigDecimal.valueOf(minimaleDagCapaciteitMinderValideAfspraken)) >= 0;
		}

		private MammaKandidaatAfspraak getKandidaatAfspraak(BigDecimal benodigdeCapaciteit)
		{
			benutteCapaciteit = benutteCapaciteit.add(benodigdeCapaciteit);

			var determinatieBlok = MammaCapaciteitZoeken.elementMetRelatiefMeesteVrijeCapaciteit(determinatieBlokList);
			LOG.debug("getKandidaatAfspraak op blok vanaf: {}, mv: {}, beschikbaar: {}, benut: {}, ratio: {}, blokId: {} ", determinatieBlok.capaciteitBlokDto.vanaf,
				determinatieBlok.capaciteitBlokDto.minderValideAfspraakMogelijk, determinatieBlok.beschikbareCapaciteit, determinatieBlok.benutteCapaciteit,
				determinatieBlok.getRatioTekst(), determinatieBlok.capaciteitBlokDto.id);
			var kandidaatAfspraak = determinatieBlok.getKandidaatAfspraak(benodigdeCapaciteit);
			if (kandidaatAfspraak.isMinderValide() && !genoegDagCapaciteitVoorMinderValide)
			{
				kandidaatAfspraak.setGeldigeAfspraak(false);
			}
			return kandidaatAfspraak;
		}

		private void bepaalDoelCapaciteit(BigDecimal aflopendPerDag)
		{
			BigDecimal doel;
			if (datum.isBefore(aflopendVanaf))
			{
				doel = BigDecimal.ONE;
			}
			else
			{
				doel = BigDecimal.ONE.subtract(aflopendPerDag.multiply(aflopendDag));

				if (doel.compareTo(BigDecimal.ZERO) <= 0)
				{
					doel = new BigDecimal("0.0001");
				}
			}

			doelCapaciteit = beschikbareCapaciteit.multiply(doel);
		}

		@Override
		public BigDecimal getDeeltal()
		{
			return benutteCapaciteit;
		}

		@Override
		public BigDecimal getDeler()
		{
			return doelCapaciteit;
		}
	}

	private class DeterminatieBlok extends MammaRationaal
	{
		private final MammaCapaciteitBlokDto capaciteitBlokDto;

		private final LocalDate datum;

		private BigDecimal beschikbareCapaciteit;

		private BigDecimal benutteCapaciteit = BigDecimal.ZERO;

		private final MammaCapaciteitBlokType blokType;

		private final List<MammaKandidaatAfspraak> kandidaatAfspraakList = new ArrayList<>();

		private final boolean blokHeeftAlMinderValideAfspraak;

		private final boolean minderValideAfspraakMogelijk;

		private final boolean heeftVrijeCapaciteit;

		@Getter
		private final List<DeterminatieMinderValidePeriode> minderValidePeriodeList = new ArrayList<>();

		private DeterminatieBlok(MammaCapaciteitBlokDto capaciteitBlokDto, LocalDate datum)
		{
			this.capaciteitBlokDto = capaciteitBlokDto;
			this.datum = datum;
			blokType = capaciteitBlokDto.blokType;
			minderValideAfspraakMogelijk = capaciteitBlokDto.minderValideAfspraakMogelijk;

			LOG.debug("{} afspraken in capaciteitBlok {} vanaf {}, beschikbaar: {}, vrij: {}, MV: {}", capaciteitBlokDto.afspraakDtos.size(), capaciteitBlokDto.id,
				capaciteitBlokDto.vanaf, capaciteitBlokDto.beschikbareCapaciteit, capaciteitBlokDto.vrijeCapaciteit, capaciteitBlokDto.minderValideAfspraakMogelijk);

			if (isMindervalide)
			{
				maakMindervalidePeriodesEnBepaalBeschikbareCapaciteit();
			}
			else
			{
				beschikbareCapaciteit = capaciteitBlokDto.beschikbareCapaciteit;

				capaciteitBlokDto.afspraakDtos.forEach(afspraak ->
				{
					var kandidaatAfspraak = new MammaKandidaatAfspraak(capaciteitBlokDto, TimeRange.of(capaciteitBlokDto.vanaf.toLocalTime(), capaciteitBlokDto.tot),
						TimeRange.of(afspraak.getVanaf().toLocalTime(), afspraak.getTot()), afspraak.getBenodigdeCapaciteit(), screeningsEenheidDto,
						afspraak.isMinderValide(), afspraak.isDubbeleTijd());
					kandidaatAfspraak.setGeldigeAfspraak(true);
					addKandidaatAfspraak(kandidaatAfspraak);
					benutteCapaciteit = benutteCapaciteit.add(afspraak.getBenodigdeCapaciteit());
				});
			}

			blokHeeftAlMinderValideAfspraak = kandidaatAfspraakList.stream().anyMatch(MammaKandidaatAfspraak::isMinderValide);
			heeftVrijeCapaciteit = beschikbareCapaciteit.subtract(benutteCapaciteit).compareTo(BigDecimal.ZERO) > 0;
		}

		private void maakMindervalidePeriodesEnBepaalBeschikbareCapaciteit()
		{
			var beschikbareCapaciteitMindervalidePeriodes = BigDecimal.ZERO;
			beschikbareCapaciteitMindervalidePeriodes = beschikbareCapaciteitMindervalidePeriodes.add(maakMindervalidePeriode(screeningsEenheidDto.getMinderValidePeriode1()));
			beschikbareCapaciteitMindervalidePeriodes = beschikbareCapaciteitMindervalidePeriodes.add(maakMindervalidePeriode(screeningsEenheidDto.getMinderValidePeriode2()));
			beschikbareCapaciteit = beschikbareCapaciteitMindervalidePeriodes;
		}

		private BigDecimal maakMindervalidePeriode(TimeRange mindervalidePeriode)
		{
			var beschikbareCapaciteitMindervalidePeriode = BigDecimal.ZERO;
			var capaciteitBlokTimeRange = TimeRange.of(capaciteitBlokDto.vanaf.toLocalTime(), capaciteitBlokDto.tot);
			if (minderValideAfspraakMogelijk && mindervalidePeriode != null && mindervalidePeriode.heeftOverlap(capaciteitBlokTimeRange))
			{
				beschikbareCapaciteitMindervalidePeriode = bepaalMindervalidePeriodeBinnenBlok(mindervalidePeriode, capaciteitBlokTimeRange);
			}
			return beschikbareCapaciteitMindervalidePeriode;
		}

		private BigDecimal bepaalMindervalidePeriodeBinnenBlok(TimeRange mindervalidePeriode, TimeRange capaciteitBlokTimeRange)
		{
			var overlappendePeriode = mindervalidePeriode.overlappendePeriode(capaciteitBlokTimeRange);
			var durationBlok = capaciteitBlokTimeRange.getDurationInMinutes();
			var durationPeriode = overlappendePeriode.getDurationInMinutes();
			var beschikbareCapaciteitMindervalidePeriode = capaciteitBlokDto.beschikbareCapaciteit.multiply(durationPeriode.divide(durationBlok, 4, RoundingMode.HALF_UP));

			maakDeterminatieMindervalidePeriode(overlappendePeriode, beschikbareCapaciteitMindervalidePeriode);
			return beschikbareCapaciteitMindervalidePeriode;
		}

		private void maakDeterminatieMindervalidePeriode(TimeRange mindervalidePeriode, BigDecimal beschikbareCapaciteitMindervalidePeriode)
		{
			var afsprakenMindervalidePeriode = new ArrayList<MammaKandidaatAfspraak>();
			capaciteitBlokDto.afspraakDtos.forEach(afspraakDto ->
			{
				if (mindervalidePeriode.bevat(afspraakDto.getVanaf().toLocalTime()))
				{
					var kandidaatAfspraak = new MammaKandidaatAfspraak(capaciteitBlokDto, TimeRange.of(mindervalidePeriode.getVanaf(), mindervalidePeriode.getTot()),
						TimeRange.of(afspraakDto.getVanaf().toLocalTime(), afspraakDto.getTot()), afspraakDto.getBenodigdeCapaciteit(),
						screeningsEenheidDto, afspraakDto.isMinderValide(), afspraakDto.isDubbeleTijd());
					kandidaatAfspraak.setGeldigeAfspraak(true);
					afsprakenMindervalidePeriode.add(kandidaatAfspraak);
					addKandidaatAfspraak(kandidaatAfspraak);
					benutteCapaciteit = benutteCapaciteit.add(afspraakDto.getBenodigdeCapaciteit());
				}
			});
			minderValidePeriodeList.add(new DeterminatieMinderValidePeriode(capaciteitBlokDto, mindervalidePeriode.getVanaf(), mindervalidePeriode.getTot(),
				afsprakenMindervalidePeriode, beschikbareCapaciteitMindervalidePeriode));
		}

		private MammaKandidaatAfspraak getKandidaatAfspraak(BigDecimal benodigdeCapaciteit)
		{
			benutteCapaciteit = benutteCapaciteit.add(benodigdeCapaciteit);
			MammaKandidaatAfspraak kandidaatAfspraak;
			if (isMindervalide)
			{
				kandidaatAfspraak = MammaCapaciteitZoeken.elementMetRelatiefMeesteVrijeCapaciteit(minderValidePeriodeList).getKandidaatAfspraak(benodigdeCapaciteit);
			}
			else
			{
				if (!datum.isBefore(aflopendVanaf))
				{
					var blokTypeCapaciteit = capaciteit.capaciteitMap.get(blokType);
					blokTypeCapaciteit.benutteCapaciteit = blokTypeCapaciteit.benutteCapaciteit.add(benodigdeCapaciteit);
					blokTypeCapaciteit.vrijeCapaciteit = blokTypeCapaciteit.vrijeCapaciteit.subtract(benodigdeCapaciteit);
					if (blokTypeCapaciteit.vrijeCapaciteit.compareTo(BigDecimal.ZERO) < 0)
					{
						blokTypeCapaciteit.negatieveVrijeCapaciteit = blokTypeCapaciteit.vrijeCapaciteit;
					}
				}

				var capaciteitBlokVanaf = capaciteitBlokDto.vanaf;
				if (kandidaatAfspraakList.isEmpty())
				{

					kandidaatAfspraak = new MammaKandidaatAfspraak(capaciteitBlokDto, TimeRange.of(capaciteitBlokDto.vanaf.toLocalTime(), capaciteitBlokDto.tot),
						TimeRange.of(capaciteitBlokVanaf.toLocalTime(), capaciteitBlokDto.tot), benodigdeCapaciteit, screeningsEenheidDto, false, isDubbeleTijd);
				}
				else
				{
					var eersteKandidaatAfspraakVanaf = kandidaatAfspraakList.get(0).getVanaf();
					if (!eersteKandidaatAfspraakVanaf.equals(capaciteitBlokVanaf.toLocalTime()))
					{

						kandidaatAfspraak = new MammaKandidaatAfspraak(capaciteitBlokDto, TimeRange.of(capaciteitBlokDto.vanaf.toLocalTime(), capaciteitBlokDto.tot),
							TimeRange.of(capaciteitBlokVanaf.toLocalTime(), eersteKandidaatAfspraakVanaf), benodigdeCapaciteit, screeningsEenheidDto, false, isDubbeleTijd);
					}
					else
					{
						kandidaatAfspraak = MammaCapaciteitZoeken.elementMetRelatiefMeesteVrijeCapaciteit(kandidaatAfspraakList)
							.splitsNieuweKandidaatAfspraak(benodigdeCapaciteit, factor, false, isDubbeleTijd);
					}
				}
			}

			if (kandidaatAfspraak.isMinderValide() && (clientHeeftGeenMinderValideAfspraakInDitBlok() && blokHeeftAlMinderValideAfspraak || !minderValideAfspraakMogelijk)
				|| !heeftVrijeCapaciteit)
			{
				kandidaatAfspraak.setGeldigeAfspraak(false);
			}

			return addKandidaatAfspraak(kandidaatAfspraak);
		}

		private MammaKandidaatAfspraak addKandidaatAfspraak(MammaKandidaatAfspraak kandidaatAfspraak)
		{
			kandidaatAfspraakList.add(kandidaatAfspraak);
			kandidaatAfspraakList.sort(Comparator.comparing(MammaKandidaatAfspraak::getVanaf));
			return kandidaatAfspraak;
		}

		private boolean clientHeeftGeenMinderValideAfspraakInDitBlok()
		{
			return !capaciteitBlokDto.id.equals(laatsteMinderValideAfspraakCapaciteitBlokId);
		}

		@Override
		public BigDecimal getDeeltal()
		{
			return benutteCapaciteit;
		}

		@Override
		public BigDecimal getDeler()
		{
			return beschikbareCapaciteit;
		}

	}

	private class DeterminatieMinderValidePeriode extends MammaRationaal
	{

		private final MammaCapaciteitBlokDto capaciteitBlokDto;

		private final List<MammaKandidaatAfspraak> kandidaatAfspraakList;

		private final BigDecimal beschikbareCapaciteit;

		private BigDecimal benutteCapaciteit = BigDecimal.ZERO;

		private final LocalTime periodeVanaf;

		private final LocalTime periodeTot;

		private DeterminatieMinderValidePeriode(MammaCapaciteitBlokDto capaciteitBlokDto, LocalTime periodeVanaf, LocalTime periodeTot,
			List<MammaKandidaatAfspraak> kandidaatAfspraakList,
			BigDecimal beschikbareCapaciteit)
		{
			this.capaciteitBlokDto = capaciteitBlokDto;
			this.periodeVanaf = periodeVanaf;
			this.periodeTot = periodeTot;
			this.kandidaatAfspraakList = kandidaatAfspraakList;
			this.beschikbareCapaciteit = beschikbareCapaciteit;

			for (var kandidaatAfspraak : kandidaatAfspraakList)
			{
				benutteCapaciteit = benutteCapaciteit.add(kandidaatAfspraak.getBenodigdeCapaciteit());
			}
		}

		MammaKandidaatAfspraak getKandidaatAfspraak(BigDecimal benodigdeCapaciteit)
		{
			LOG.debug("getKandidaatAfspraak op MV-periode blok-vanaf: {}, periode-vanaf: {}, beschikbaar: {}, benut: {}", capaciteitBlokDto.vanaf, periodeVanaf,
				beschikbareCapaciteit, benutteCapaciteit);

			benutteCapaciteit = benutteCapaciteit.add(benodigdeCapaciteit);
			MammaKandidaatAfspraak kandidaatAfspraak;
			if (kandidaatAfspraakList.isEmpty())
			{

				kandidaatAfspraak = new MammaKandidaatAfspraak(capaciteitBlokDto, TimeRange.of(periodeVanaf, periodeTot),
					TimeRange.of(periodeVanaf, periodeTot), benodigdeCapaciteit, screeningsEenheidDto, true, isDubbeleTijd);
			}
			else
			{
				var eersteKandidaatAfspraakVanaf = kandidaatAfspraakList.get(0).getVanaf();
				if (!eersteKandidaatAfspraakVanaf.equals(periodeVanaf))
				{

					kandidaatAfspraak = new MammaKandidaatAfspraak(capaciteitBlokDto, TimeRange.of(periodeVanaf, periodeTot),
						TimeRange.of(periodeVanaf, eersteKandidaatAfspraakVanaf), benodigdeCapaciteit, screeningsEenheidDto, true, isDubbeleTijd);
				}
				else
				{
					kandidaatAfspraak = MammaCapaciteitZoeken.elementMetRelatiefMeesteVrijeCapaciteit(kandidaatAfspraakList)
						.splitsNieuweKandidaatAfspraak(benodigdeCapaciteit, factor, true, isDubbeleTijd);
				}
			}

			return addKandidaatAfspraak(kandidaatAfspraak);
		}

		private MammaKandidaatAfspraak addKandidaatAfspraak(MammaKandidaatAfspraak kandidaatAfspraak)
		{
			kandidaatAfspraakList.add(kandidaatAfspraak);
			kandidaatAfspraakList.sort(Comparator.comparing(MammaKandidaatAfspraak::getVanaf));
			return kandidaatAfspraak;
		}

		@Override
		public BigDecimal getDeeltal()
		{
			return benutteCapaciteit;
		}

		@Override
		public BigDecimal getDeler()
		{
			return beschikbareCapaciteit;
		}
	}

}
