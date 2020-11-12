package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;
import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaScreeningsEenheidDto;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseKandidaatAfsprakenDeterminatiePeriode;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
@Scope("prototype")
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseKandidaatAfsprakenDeterminatiePeriodeImpl implements MammaBaseKandidaatAfsprakenDeterminatiePeriode
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseKandidaatAfsprakenDeterminatiePeriodeImpl.class);

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaBaseCapaciteitsBlokService capaciteitsBlokService;

	@Autowired
	private MammaBaseDossierService dossierService;

	private static final BigDecimal TEN = new BigDecimal(10);

	private static final BigDecimal MINUS_HALF = new BigDecimal(-0.5);

	private static final BigDecimal MINIMUM_OPKOMSTKANS = new BigDecimal(0.1);

	private boolean meerdereKandidaten = false;

	private List<MammaStandplaatsPeriode> standplaatsPeriodeList;

	private Map<MammaStandplaatsPeriode, LocalDate> standplaatsPeriodeVanafMap = new HashMap<>(), standplaatsPeriodeTotEnMetMap = new HashMap<>();

	private MammaDossier dossier;

	private BigDecimal factor;

	private BigDecimal benodigdeCapaciteit;

	private boolean extraOpties = false;

	private LocalDate vandaagOfMorgen, aflopendVanaf;

	private MammaCapaciteit capaciteit;

	private MammaCapaciteitBlokType blokType;

	private Map<LocalDate, List<MammaCapaciteitBlokDto>> dateCapaciteitBlokMap = new TreeMap<>();

	private List<DeterminatieDag> determinatieDagList = new ArrayList<>();

	private MammaScreeningsEenheidDto screeningsEenheidDto;

	private int minimaleDagCapaciteitMinderValideAfspraken;

	private Long laatsteMinderValideAfspraakCapaciteitBlokId;

	private void init(List<MammaStandplaatsPeriode> standplaatsPeriodeList, MammaDossier dossier, BigDecimal voorlopigeOpkomstkans,
		Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen)
	{
		this.standplaatsPeriodeList = standplaatsPeriodeList;
		this.dossier = dossier;

		MammaStandplaatsPeriode standplaatsPeriode = standplaatsPeriodeList.get(0);

		MammaScreeningsEenheid screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
		screeningsEenheidDto = MammaScreeningsEenheidDto.maakScreeningsEenheid(screeningsEenheid);

		ScreeningOrganisatie screeningOrganisatie = (ScreeningOrganisatie) HibernateHelper.deproxy(screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio());
		factor = dossierService.getFactorType(dossier).getFactor(screeningOrganisatie);
		minimaleDagCapaciteitMinderValideAfspraken = screeningOrganisatie.getMinimaleDagCapaciteitMinderValideAfspraken();

		benodigdeCapaciteit = factor.multiply(voorlopigeOpkomstkans.compareTo(MINIMUM_OPKOMSTKANS) >= 0 ? voorlopigeOpkomstkans : MINIMUM_OPKOMSTKANS);

		LocalDateTime nu = dateSupplier.getLocalDateTime();
		vandaagOfMorgen = nu.toLocalTime().isBefore(Constants.BK_EINDTIJD_DAG) ? nu.toLocalDate() : nu.toLocalDate().plusDays(1);

		aflopendVanaf = DateUtil.plusWerkdagen(vandaagOfMorgen, capaciteitVolledigBenutTotEnMetAantalWerkdagen);

		if (dossier.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE))
		{
			MammaAfspraak laatsteAfspraak = dossier.getLaatsteScreeningRonde() != null ? MammaScreeningRondeUtil.getLaatsteAfspraak(dossier.getLaatsteScreeningRonde()) : null;
			if (laatsteAfspraak != null && laatsteAfspraak.getCapaciteitBlok() != null)
			{
				laatsteMinderValideAfspraakCapaciteitBlokId = laatsteAfspraak.getCapaciteitBlok().getId();
			}
		}
	}

	@Override
	public List<MammaKandidaatAfspraak> getKandidaatAfspraken(MammaDossier dossier, MammaStandplaatsPeriode standplaatsPeriode, LocalDate vanaf, LocalDate totEnMet,
		boolean extraOpties, BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen, boolean corrigeerNegatieveVrijeCapaciteit)
	{
		init(Arrays.asList(standplaatsPeriode), dossier, voorlopigeOpkomstkans, capaciteitVolledigBenutTotEnMetAantalWerkdagen);
		this.extraOpties = extraOpties;

		this.standplaatsPeriodeVanafMap.put(standplaatsPeriode, vanaf);
		this.standplaatsPeriodeTotEnMetMap.put(standplaatsPeriode, totEnMet);

		meerdereKandidaten = true;
		return getKandidaatAfspraken(corrigeerNegatieveVrijeCapaciteit);
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
		List<MammaStandplaatsPeriode> standplaatsPeriodeList = standplaatsRonde.getStandplaatsPerioden().stream()
			.filter(standplaatsPeriode -> standplaatsPeriode.getScreeningsEenheid().getUitnodigenTotEnMet() != null).collect(Collectors.toList());
		init(standplaatsPeriodeList, dossier, voorlopigeOpkomstkans, capaciteitVolledigBenutTotEnMetAantalWerkdagen);

		LocalDate uitnodigenVanafDatum = DateUtil.plusWerkdagen(vandaagOfMorgen, afspraakBijUitnodigenVanafAantalWerkdagen);
		for (MammaStandplaatsPeriode standplaatsPeriode : standplaatsPeriodeList)
		{
			standplaatsPeriodeVanafMap.put(standplaatsPeriode, Collections.max(Arrays.asList(uitnodigenVanafDatum, DateUtil.toLocalDate(standplaatsPeriode.getVanaf()))));
			standplaatsPeriodeTotEnMetMap.put(standplaatsPeriode,
				DateUtil.toLocalDate(Collections.min(Arrays.asList(standplaatsPeriode.getScreeningsEenheid().getUitnodigenTotEnMet(), standplaatsPeriode.getTotEnMet()))));
		}

		List<MammaKandidaatAfspraak> kandidaatAfspraken = getKandidaatAfspraken(false);
		if (kandidaatAfspraken.isEmpty())
		{
			throw new MammaOnvoldoendeVrijeCapaciteitException();
		}
		return kandidaatAfspraken.get(0);
	}

	private void bepaalDeterminatiePeriode()
	{
		Collection<MammaCapaciteitBlokDto> nietGeblokkeerdeCapaciteitsBlokken = new ArrayList<>();

		for (MammaStandplaatsPeriode standplaatsPeriode : standplaatsPeriodeList)
		{
			Date vanafDate = DateUtil.toUtilDate(standplaatsPeriodeVanafMap.get(standplaatsPeriode));
			Date totEnMetDate = DateUtil.toUtilDate(standplaatsPeriodeTotEnMetMap.get(standplaatsPeriode).atTime(Constants.BK_EINDTIJD_DAG));

			Collection<MammaCapaciteitBlokDto> standplaatsPeriodeNietGeblokkeerdeCapaciteitsBlokken = capaciteitsBlokService.getNietGeblokkerdeCapaciteitsBlokDtos(
				standplaatsPeriode,
				vanafDate, totEnMetDate, blokType == null ? null : Collections.singletonList(blokType));
			standplaatsPeriodeNietGeblokkeerdeCapaciteitsBlokken.forEach(capaciteitBlok -> capaciteitBlok.standplaatsPeriode = standplaatsPeriode);

			nietGeblokkeerdeCapaciteitsBlokken.addAll(standplaatsPeriodeNietGeblokkeerdeCapaciteitsBlokken);
		}

		blokType = dossier.getTehuis() == null ? MammaCapaciteitBlokType.REGULIER : MammaCapaciteitBlokType.TEHUIS;

		capaciteit = capaciteitsBlokService.getCapaciteit(nietGeblokkeerdeCapaciteitsBlokken);

		TreeMap<LocalDate, List<MammaCapaciteitBlokDto>> capaciteitsBlokMap = new TreeMap<>();
		nietGeblokkeerdeCapaciteitsBlokken.stream()
			.filter(blok -> blok.blokType == blokType)
			.forEach(blok -> {
				LocalDate vanafDatum = blok.vanaf.toLocalDate();

				List<MammaCapaciteitBlokDto> capaciteitBlokList = capaciteitsBlokMap.get(vanafDatum);
				if (capaciteitBlokList == null)
				{
					capaciteitBlokList = new ArrayList<>();
					capaciteitsBlokMap.put(vanafDatum, capaciteitBlokList);
				}
				capaciteitBlokList.add(blok);
			});

		capaciteitsBlokMap.forEach((key, value) -> value.sort(Comparator.comparing(b -> b.vanaf)));

		dateCapaciteitBlokMap = capaciteitsBlokMap;
		determinatieDagList = capaciteitsBlokMap.keySet().stream()
			.map(DeterminatieDag::new)
			.filter(determinatieDag -> determinatieDag.beschikbareCapaciteit.subtract(determinatieDag.benutteCapaciteit).compareTo(BigDecimal.ZERO) >= 0)
			.collect(Collectors.toList());
	}

	private List<MammaKandidaatAfspraak> getKandidaatAfspraken(boolean corrigeerNegatieveVrijeCapaciteit)
	{
		bepaalDeterminatiePeriode();

		MammaCapaciteit.BlokTypeCapaciteit totaleCapaciteit = capaciteit.getCapaciteit(blokType);

		List<MammaKandidaatAfspraak> kandidaatAfspraken = new ArrayList<>();
		if (dossier.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE) && standplaatsPeriodeList.get(0).getStandplaatsRonde().getMinderValideUitwijkStandplaats() != null)
		{
			return kandidaatAfspraken;
		}

		BigDecimal totaleVrijeCapaciteit = totaleCapaciteit.getVrijeCapaciteit(corrigeerNegatieveVrijeCapaciteit);

		if (meerdereKandidaten && totaleVrijeCapaciteit.subtract(benodigdeCapaciteit).compareTo(MINUS_HALF) <= 0)
		{
			return kandidaatAfspraken;
		}

		int aantalKandidatenVrijeCapaciteit = totaleVrijeCapaciteit.divide(benodigdeCapaciteit, 5, BigDecimal.ROUND_HALF_UP).setScale(0, BigDecimal.ROUND_UP)
			.intValue();
		int maxAantalKandidatenTonen = totaleCapaciteit.beschikbareCapaciteit.divide(TEN, 5, BigDecimal.ROUND_HALF_UP).setScale(0, BigDecimal.ROUND_UP).intValue();

		Map<MammaCapaciteitBlokType, Integer> aantalKandidatenBlokTypeMap = new HashMap<>();

		MammaCapaciteit.BlokTypeCapaciteit blokTypeCapaciteit = this.capaciteit.getCapaciteit(blokType);
		BigDecimal vrijeCapaciteit = blokTypeCapaciteit.getVrijeCapaciteit(corrigeerNegatieveVrijeCapaciteit);

		aantalKandidatenBlokTypeMap.put(blokType,
			vrijeCapaciteit.divide(benodigdeCapaciteit, 5, BigDecimal.ROUND_HALF_UP).setScale(0, BigDecimal.ROUND_DOWN).intValue());

		aantalKandidatenBlokTypeMap.put(blokType, aantalKandidatenBlokTypeMap.get(blokType) + 1);

		for (int i = 0; i < aantalKandidatenBlokTypeMap.get(blokType); i++)
		{
			if (aantalKandidatenVrijeCapaciteit == 0 || (!extraOpties && maxAantalKandidatenTonen == 0) || (!meerdereKandidaten && kandidaatAfspraken.size() > 0))
			{
				break;
			}

			bepaalDoelCapaciteit(blokType, corrigeerNegatieveVrijeCapaciteit);
			MammaKandidaatAfspraak kandidaatAfspraak = binairZoeken(determinatieDagList).getKandidaatAfspraak(benodigdeCapaciteit);
			aantalKandidatenVrijeCapaciteit--;
			if (kandidaatAfspraak.isValideAfspraak())
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
		MammaCapaciteit.BlokTypeCapaciteit blokTypeCapaciteit = this.capaciteit.getCapaciteit(blokType);
		if (blokTypeCapaciteit.getVrijeCapaciteit(corrigeerNegatieveVrijeCapaciteit).compareTo(BigDecimal.ZERO) <= 0)
		{
			aflopendPerDag = BigDecimal.ZERO;
		}
		else
		{
			BigDecimal totaleCapaciteitInAflopendePeriode = BigDecimal.ZERO;
			BigDecimal beschikbareCapaciteitInAflopendePeriode = BigDecimal.ZERO;
			BigDecimal vrijeCapaciteitInAflopendePeriode = BigDecimal.ZERO;

			for (DeterminatieDag determinatieDag : determinatieDagList)
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
				BigDecimal aflopendGemiddeldeDag = totaleCapaciteitInAflopendePeriode.divide(beschikbareCapaciteitInAflopendePeriode, 10, RoundingMode.HALF_UP);

				if (aflopendGemiddeldeDag.compareTo(BigDecimal.ZERO) != 0)
				{
					BigDecimal aflopendGemiddeldeDoel = vrijeCapaciteitInAflopendePeriode.divide(beschikbareCapaciteitInAflopendePeriode, 10, RoundingMode.HALF_UP);

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
		private LocalDate datum;

		private BigDecimal beschikbareCapaciteit = BigDecimal.ZERO;

		private BigDecimal benutteCapaciteit = BigDecimal.ZERO;

		private BigDecimal doelCapaciteit = BigDecimal.ZERO;

		private List<DeterminatieBlok> determinatieBlokList = new ArrayList<>();

		private BigDecimal aflopendDag;

		private DeterminatieDag(LocalDate datum)
		{
			this.datum = datum;
			this.aflopendDag = new BigDecimal(ChronoUnit.DAYS.between(aflopendVanaf, datum.plusDays(1L)));

			dateCapaciteitBlokMap.get(datum)
				.forEach(capaciteitBlok -> {
					DeterminatieBlok determinatieBlok = new DeterminatieBlok(capaciteitBlok, datum);
					determinatieBlokList.add(determinatieBlok);

					beschikbareCapaciteit = beschikbareCapaciteit.add(determinatieBlok.beschikbareCapaciteit);
					benutteCapaciteit = benutteCapaciteit.add(determinatieBlok.benutteCapaciteit);
				});
		}

		private MammaKandidaatAfspraak getKandidaatAfspraak(BigDecimal benodigdeCapaciteit)
		{
			benutteCapaciteit = benutteCapaciteit.add(benodigdeCapaciteit);

			MammaKandidaatAfspraak kandidaatAfspraak = binairZoeken(determinatieBlokList).getKandidaatAfspraak(benodigdeCapaciteit);
			if (kandidaatAfspraak.isMinderValide() && beschikbareCapaciteit.compareTo(BigDecimal.valueOf(minimaleDagCapaciteitMinderValideAfspraken)) < 0)
			{
				kandidaatAfspraak.setValideAfspraak(false);
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
					doel = new BigDecimal(0.0001);
				}
			}

			this.doelCapaciteit = beschikbareCapaciteit.multiply(doel);
		}

		@Override
		BigDecimal getDeeltal()
		{
			return benutteCapaciteit;
		}

		@Override
		BigDecimal getDeler()
		{
			return doelCapaciteit;
		}
	}

	private class DeterminatieBlok extends MammaRationaal
	{
		private MammaCapaciteitBlokDto capaciteitBlokDto;

		private LocalDate datum;

		private BigDecimal beschikbareCapaciteit;

		private BigDecimal benutteCapaciteit = BigDecimal.ZERO;

		private MammaCapaciteitBlokType blokType;

		private List<MammaKandidaatAfspraak> kandidaatAfspraakList = new ArrayList<>();

		private boolean heeftMinderValideAfspraak;

		private boolean minderValideAfspraakMogelijk;

		private boolean heeftVrijeCapaciteit;

		private DeterminatieBlok(MammaCapaciteitBlokDto capaciteitBlokDto, LocalDate datum)
		{
			this.capaciteitBlokDto = capaciteitBlokDto;
			this.datum = datum;
			this.beschikbareCapaciteit = capaciteitBlokDto.beschikbareCapaciteit;
			blokType = capaciteitBlokDto.blokType;

			LOG.debug(capaciteitBlokDto.afspraakDtos.size() + " afspraken in capaciteitBlok " + capaciteitBlokDto.id);
			capaciteitBlokDto.afspraakDtos.forEach(afspraak -> {
				MammaKandidaatAfspraak kandidaatAfspraak = new MammaKandidaatAfspraak(capaciteitBlokDto, afspraak.vanaf.toLocalDate(), afspraak.vanaf.toLocalTime(),
					afspraak.tot, afspraak.benodigdeCapaciteit, screeningsEenheidDto, afspraak.minderValide);
				kandidaatAfspraak.setValideAfspraak(true);
				addKandidaatAfspraak(kandidaatAfspraak);
				benutteCapaciteit = benutteCapaciteit.add(afspraak.benodigdeCapaciteit);
			});

			heeftMinderValideAfspraak = kandidaatAfspraakList.stream().anyMatch(MammaKandidaatAfspraak::isMinderValide);
			minderValideAfspraakMogelijk = capaciteitBlokDto.minderValideAfspraakMogelijk;
			heeftVrijeCapaciteit = beschikbareCapaciteit.subtract(benutteCapaciteit).compareTo(BigDecimal.ZERO) > 0;
		}

		private MammaKandidaatAfspraak getKandidaatAfspraak(BigDecimal benodigdeCapaciteit)
		{
			benutteCapaciteit = benutteCapaciteit.add(benodigdeCapaciteit);
			if (!datum.isBefore(aflopendVanaf))
			{
				MammaCapaciteit.BlokTypeCapaciteit blokTypeCapaciteit = capaciteit.capaciteitMap.get(blokType);
				blokTypeCapaciteit.benutteCapaciteit = blokTypeCapaciteit.benutteCapaciteit.add(benodigdeCapaciteit);
				blokTypeCapaciteit.vrijeCapaciteit = blokTypeCapaciteit.vrijeCapaciteit.subtract(benodigdeCapaciteit);
				if (blokTypeCapaciteit.vrijeCapaciteit.compareTo(BigDecimal.ZERO) < 0)
				{
					blokTypeCapaciteit.negatieveVrijeCapaciteit = blokTypeCapaciteit.vrijeCapaciteit;
				}
			}

			MammaKandidaatAfspraak kandidaatAfspraak;
			LocalDateTime capaciteitBlokVanaf = capaciteitBlokDto.vanaf;
			if (kandidaatAfspraakList.isEmpty())
			{

				kandidaatAfspraak = new MammaKandidaatAfspraak(capaciteitBlokDto, capaciteitBlokVanaf.toLocalDate(), capaciteitBlokVanaf.toLocalTime(),
					capaciteitBlokDto.tot, benodigdeCapaciteit, screeningsEenheidDto, dossier.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE));
			}
			else
			{
				LocalTime eersteKandidaatAfspraakVanaf = kandidaatAfspraakList.get(0).getVanaf();
				if (!eersteKandidaatAfspraakVanaf.equals(capaciteitBlokVanaf.toLocalTime()))
				{

					kandidaatAfspraak = new MammaKandidaatAfspraak(capaciteitBlokDto, capaciteitBlokVanaf.toLocalDate(), capaciteitBlokVanaf.toLocalTime(),
						eersteKandidaatAfspraakVanaf, benodigdeCapaciteit, screeningsEenheidDto, dossier.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE));
				}
				else
				{
					kandidaatAfspraak = binairZoeken(kandidaatAfspraakList).getKandidaatAfspraak(benodigdeCapaciteit, factor,
						dossier.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE));
				}
			}

			if ((kandidaatAfspraak.isMinderValide() && ((clientHeeftGeenMinderValideAfspraakInDitBlok() && heeftMinderValideAfspraak) || !minderValideAfspraakMogelijk))
				|| !heeftVrijeCapaciteit)
			{
				kandidaatAfspraak.setValideAfspraak(false);
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
		BigDecimal getDeeltal()
		{
			return benutteCapaciteit;
		}

		@Override
		BigDecimal getDeler()
		{
			return beschikbareCapaciteit;
		}
	}

	private <R extends MammaRationaal> R binairZoeken(List<R> list)
	{
		if (list.size() > 2)
		{
			int middle = list.size() / 2;

			if (list.get(0) instanceof MammaKandidaatAfspraak)
			{
				Integer midden = gecorrigeerdMidden((List<MammaKandidaatAfspraak>) list, middle);
				if (midden != null)
				{
					middle = midden;
				}
				else
				{
					return list.get(list.size() - 1);
				}
			}

			List<R> list1 = list.subList(0, middle);
			List<R> list2 = list.subList(middle, list.size());

			MammaRationaal rationaal1 = MammaRationaal.getRationaal(list1);
			MammaRationaal rationaal2 = MammaRationaal.getRationaal(list2);

			if (rationaal1.compareTo(rationaal2) <= 0)
			{
				return binairZoeken(list1);
			}
			else
			{
				return binairZoeken(list2);
			}
		}
		else
		{
			return Collections.min(list);
		}
	}

	private Integer gecorrigeerdMidden(List<MammaKandidaatAfspraak> afspraken, int middle)
	{
		LocalTime vanafMiddle = afspraken.get(middle).getVanaf();
		int index = middle - 1;
		boolean forward = true;
		for (int i = 2; 0 <= index && index < afspraken.size(); i++)
		{
			LocalTime vanafIndex = afspraken.get(index).getVanaf();
			if (!vanafMiddle.equals(vanafIndex))
			{
				return forward ? index + 1 : index;
			}
			index = forward ? index + i : index - i;
			forward = !forward;
		}
		return null;
	}
}