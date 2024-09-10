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
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.MammaDagEnDagdeelFilter;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.BeschikbareAfspraakDagen;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Dagdeel;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.SmsStatus;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelGeannuleerdReden;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.repository.mamma.MammaBaseAfspraakRepository;
import nl.rivm.screenit.repository.mamma.MammaCapaciteitBlokRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.BerichtToSeRestBkService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseKandidaatAfsprakenDeterminatiePeriode;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.tuple.Pair;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.begintTussen;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.filterStatuses;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftClientInTehuis;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftDoelgroep;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftGeenCapaciteitBlok;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftGeenClientInTehuis;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftScreeningsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftStandplaats;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftStandplaatsPeriode;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftStatuses;

@Service
@Slf4j
public class MammaBaseAfspraakServiceImpl implements MammaBaseAfspraakService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseStandplaatsService standplaatsService;

	@Autowired
	private MammaBaseUitstelService uitstelService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private MammaBaseDossierService dossierService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Autowired
	private LogService logService;

	@Autowired
	private BerichtToSeRestBkService berichtToSeRestBkService;

	@Autowired
	private MammaBaseKansberekeningService kansberekeningService;

	@Autowired
	private MammaBaseFactory baseFactory;

	@Autowired
	private BaseBriefService baseBriefService;

	@Autowired
	private MammaBaseAfspraakRepository baseAfspraakRepository;

	@Autowired
	private MammaCapaciteitBlokRepository capaciteitBlokRepository;

	@Override
	public List<MammaKandidaatAfspraakDto> getKandidaatAfspraken(Client client, IMammaAfspraakWijzigenFilter filter)
	{
		var kandidaatAfspraakDtos = new ArrayList<MammaKandidaatAfspraakDto>();
		var dossier = client.getMammaDossier();
		var minimaleTijdstip = currentDateSupplier.getLocalDateTime()
			.plusMinutes(preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_ZOEKEN_AANTAL_MINUTEN_IN_TOEKOMST.name(), 0));

		var capaciteitVolledigBenutTotEnMetAantalWerkdagen = preferenceService.getInteger(PreferenceKey.MAMMA_CAPACITEIT_VOLLEDIG_BENUT_TOT_EN_MET_AANTAL_WERKDAGEN.toString());
		var minimaleIntervalMammografieOnderzoeken = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN.name());
		var standplaatsPeriodeMetAfstandDtos = standplaatsService.getStandplaatsPeriodeMetAfstandDtos(client, filter);

		for (var standplaatsPeriodeMetAfstandDto : standplaatsPeriodeMetAfstandDtos)
		{
			var standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, standplaatsPeriodeMetAfstandDto.getStandplaatsPeriodeId());
			var laatsteUitnodiging = dossier.getLaatsteScreeningRonde() != null ? dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() : null;
			var voorlopigeOpkomstkans = laatsteUitnodiging != null
				? kansberekeningService.getVoorlopigeOpkomstkans(laatsteUitnodiging, standplaatsPeriode, filter.getVerzettenReden())
				: kansberekeningService.getVoorlopigeOpkomstkans(dossier, standplaatsPeriode, filter.getVerzettenReden(), BriefType.MAMMA_AFSPRAAK_UITNODIGING);
			var vrijgegevenTotEnMetDatum = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			if (vrijgegevenTotEnMetDatum != null)
			{
				var standplaatsPeriodeVanaf = DateUtil.toLocalDate(standplaatsPeriode.getVanaf());
				var standplaatsPeriodeTotEnMet = DateUtil.toLocalDate(standplaatsPeriode.getTotEnMet());

				var vanafDatum = Collections.max(Arrays.asList(filter.getVanaf(), standplaatsPeriodeVanaf));
				var totEnMetDatum = Collections.min(Arrays.asList(filter.getTotEnMet(), vrijgegevenTotEnMetDatum, standplaatsPeriodeTotEnMet));
				var baseKandidaatAfsprakenDeterminatiePeriode = createKandidaatAfsprakenDeterminatiePeriodeService();
				var kandidaatAfsprakenStandplaatsPeriode = baseKandidaatAfsprakenDeterminatiePeriode.getKandidaatAfspraken(dossier, standplaatsPeriode,
					vroegstMogelijkeUitnodigingsDatum(dossier, vanafDatum, minimaleIntervalMammografieOnderzoeken), totEnMetDatum, filter.getExtraOpties(), voorlopigeOpkomstkans,
					capaciteitVolledigBenutTotEnMetAantalWerkdagen, true);

				kandidaatAfsprakenStandplaatsPeriode.forEach(kandidaatAfspraak ->
				{
					if (minimaleTijdstip.isBefore(kandidaatAfspraak.getDatumTijd()))
					{
						kandidaatAfspraakDtos
							.add(new MammaKandidaatAfspraakDto(kandidaatAfspraak.getCapaciteitBlokDto().id, kandidaatAfspraak.getDatum(), kandidaatAfspraak.getVanaf(),
								standplaatsPeriode.getId(), standplaatsPeriodeMetAfstandDto.getAfstand()));
					}
				});
			}
		}
		return kandidaatAfspraakDtos;
	}

	private static MammaBaseKandidaatAfsprakenDeterminatiePeriode createKandidaatAfsprakenDeterminatiePeriodeService()
	{
		return ApplicationContextProvider.getApplicationContext().getBean(MammaBaseKandidaatAfsprakenDeterminatiePeriode.class);
	}

	@Override
	public List<MammaKandidaatAfspraakDto> filterKandidaatAfsprakenOpDagEnDagdeel(List<MammaKandidaatAfspraakDto> afspraken, MammaDagEnDagdeelFilter filter)
	{
		if (filter != null)
		{
			var startMiddag = LocalTime.parse(preferenceService.getString(PreferenceKey.START_MIDDAG.name()));
			var startAvond = LocalTime.parse(preferenceService.getString(PreferenceKey.START_AVOND.name()));
			return afspraken.stream()
				.filter(a -> voldoetAfspraakAanFilter(a, filter, startMiddag, startAvond))
				.collect(Collectors.toList());
		}
		else
		{
			return afspraken;
		}
	}

	private boolean voldoetAfspraakAanFilter(MammaKandidaatAfspraakDto afspraak, MammaDagEnDagdeelFilter filter, LocalTime startMiddag, LocalTime startAvond)
	{
		return voldoetAfspraakAanDagFilter(afspraak, filter.getDagen()) && voldoetAfspraakAanDagDeelFilter(afspraak, filter.getDagdelen(), startMiddag, startAvond);
	}

	private boolean voldoetAfspraakAanDagFilter(MammaKandidaatAfspraakDto afspraak, List<BeschikbareAfspraakDagen> keuzeDagen)
	{
		if (keuzeDagen.isEmpty())
		{
			return true;
		}
		var afspraakDag = afspraak.getDatum().getDayOfWeek();
		return keuzeDagen.stream().anyMatch(dag -> dag.getDagVanDeWeek().equals(afspraakDag));
	}

	private boolean voldoetAfspraakAanDagDeelFilter(MammaKandidaatAfspraakDto afspraak, List<Dagdeel> keuzeDagdelen, LocalTime startMiddag, LocalTime startAvond)
	{
		if (keuzeDagdelen.isEmpty())
		{
			return true;
		}
		return keuzeDagdelen.stream().anyMatch(dagdeel -> dagdeel.equals(getAfspraakDagdeel(afspraak, startMiddag, startAvond)));
	}

	private Dagdeel getAfspraakDagdeel(MammaKandidaatAfspraakDto afspraak, LocalTime startMiddag, LocalTime startAvond)
	{

		var afspraakTijd = afspraak.getTijd();
		if (!afspraakTijd.isBefore(startAvond))
		{
			return Dagdeel.AVOND;
		}
		else if (!afspraakTijd.isBefore(startMiddag))
		{
			return Dagdeel.MIDDAG;
		}
		return Dagdeel.OCHTEND;
	}

	@Override
	public boolean valideUitstelStreefDatum(LocalDate streefDatum, MammaStandplaatsPeriode standplaatsPeriode)
	{
		var standplaatsPeriodeVanaf = DateUtil.toLocalDate(standplaatsPeriode.getVanaf());
		var standplaatsPeriodeTotEnMet = DateUtil.toLocalDate(standplaatsPeriode.getTotEnMet());
		if (!standplaatsPeriodeVanaf.isAfter(streefDatum) && !standplaatsPeriodeTotEnMet.isBefore(streefDatum))
		{
			var vrijgegevenTotEnMet = DateUtil.toLocalDate(standplaatsPeriode.getScreeningsEenheid().getVrijgegevenTotEnMet());
			return vrijgegevenTotEnMet != null && streefDatum.isAfter(vrijgegevenTotEnMet);
		}
		return false;
	}

	@Override
	public LocalDate vroegstMogelijkeUitnodigingsDatum(MammaDossier dossier, LocalDate voorstelDatum, Integer minimaleIntervalMammografieOnderzoeken)
	{
		var laatsteMammografieAfgerond = dossier.getLaatsteMammografieAfgerond();
		var onderzoek = dossierService.getLaatsteOnderzoek(dossier);
		boolean heeftGeforceerdeAfspraak = dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null
			&& dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getAfspraken().stream().anyMatch(MammaAfspraak::isGeforceerdeAfspraak);
		if (laatsteMammografieAfgerond != null && onderzoek != null
			&& onderzoek.getStatus() != MammaOnderzoekStatus.ONDERBROKEN && onderzoek.getStatus() != MammaOnderzoekStatus.ONDERBROKEN_ZONDER_VERVOLG
			&& !dossierService.isAfspraakForcerenMogelijk(dossier) && !heeftGeforceerdeAfspraak)
		{
			var minimaalIntervalOnderzoeken = DateUtil.toLocalDate(laatsteMammografieAfgerond).plusDays(minimaleIntervalMammografieOnderzoeken);
			if (minimaalIntervalOnderzoeken.isAfter(voorstelDatum))
			{
				return minimaalIntervalOnderzoeken;
			}
		}
		return voorstelDatum;
	}

	@Override
	public LocalDate laatstMogelijkeAfspraakDatum(MammaDossier dossier)
	{
		if (dossier.getLaatsteScreeningRonde() != null)
		{
			var mammografieOptional = getMammografieVanEersteOnderbrokenOnderzoek(dossier);
			return mammografieOptional.map(
					mammografie -> DateUtil.toLocalDate(mammografie.getAfgerondOp()).plusMonths(Constants.MAMMA_MAX_AANTAL_MAANDEN_GEEN_UITSLAG_ONDERBROKEN_ONDERZOEK))
				.orElse(null);
		}
		return null;
	}

	private Optional<MammaMammografie> getMammografieVanEersteOnderbrokenOnderzoek(MammaDossier dossier)
	{
		var afspraken = dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getAfspraken();
		return afspraken.stream()
			.filter(this::isAfspraakOnderzoekOnderbrokenMetBeelden)
			.map(afspraak -> afspraak.getOnderzoek().getMammografie()).min(Comparator.comparing(MammaMammografie::getAfgerondOp));
	}

	private boolean isAfspraakOnderzoekOnderbrokenMetBeelden(MammaAfspraak afspraak)
	{
		var onderzoek = afspraak.getOnderzoek();
		return onderzoek != null && MammaOnderzoekStatus.ONDERBROKEN == onderzoek.getStatus() && onderzoek.getMammografie() != null
			&& MammaMammografieIlmStatus.beeldenBeschikbaarOfBeschikbaarGeweest(onderzoek.getMammografie().getIlmStatus());
	}

	@Override
	public List<MammaAfspraak> getAfspraken(MammaScreeningsEenheid screeningsEenheid, LocalDate vanaf, LocalDate totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		var afspraken = baseAfspraakRepository.findWith(
			heeftScreeningsEenheid(screeningsEenheid)
				.and(begintTussenTotEnMet(vanaf, totEnMet))
				.and(filterStatuses(Arrays.asList(afspraakStatussen))), q ->
				q.fetch(g -> g.addSubgraph(MammaAfspraak_.uitnodiging).addSubgraph(MammaUitnodiging_.screeningRonde)
						.addSubgraph(MammaScreeningRonde_.dossier).addSubgraph(MammaDossier_.client).addSubgraph(Client_.persoon))
					.all());

		bepaalBenodigdeCapaciteit(afspraken, screeningsEenheid);
		return afspraken;
	}

	@Override
	public List<MammaAfspraak> getAfspraken(String seCode, LocalDate vanaf, LocalDate totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		return baseAfspraakRepository.findAll(heeftScreeningsEenheid(seCode)
			.and(begintTussenTotEnMet(vanaf, totEnMet))
			.and(filterStatuses(Arrays.asList(afspraakStatussen))));
	}

	@Override
	public void bepaalBenodigdeCapaciteit(List<MammaAfspraak> afspraken, MammaScreeningsEenheid screeningsEenheid)
	{
		var screeningOrganisatie = (ScreeningOrganisatie) HibernateHelper.deproxy(screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio());

		for (MammaAfspraak afspraak : afspraken)
		{
			if (afspraak.getBenodigdeCapaciteit() == null) 
			{
				var dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
				var factor = dossierService.getFactorType(dossier).getFactor(screeningOrganisatie);
				var opkomstkans = afspraak.getOpkomstkans().getOpkomstkans();
				afspraak.setBenodigdeCapaciteit(factor.multiply(opkomstkans));
			}
		}
	}

	@Transactional(propagation = Propagation.MANDATORY)
	@Override
	public MammaAfspraak maakAfspraak(MammaScreeningRonde screeningRonde, MammaCapaciteitBlok capaciteitBlok, Date vanaf, MammaStandplaatsPeriode standplaatsPeriode,
		MammaVerzettenReden verzettenReden, boolean vorigeAfspraakVerzetten, boolean notificeerBetrokkenSe, boolean isBulk, boolean stuurBerichtNaarSectra, boolean logGebeurtenis,
		Account account, boolean isGeforceerdeAfspraak)
	{
		return maakAfspraak(screeningRonde, capaciteitBlok, vanaf, standplaatsPeriode, verzettenReden, vorigeAfspraakVerzetten, notificeerBetrokkenSe, isBulk,
			stuurBerichtNaarSectra,
			logGebeurtenis, account, isGeforceerdeAfspraak, SmsStatus.GEEN);
	}

	@Transactional(propagation = Propagation.MANDATORY)
	@Override
	public MammaAfspraak maakAfspraak(MammaScreeningRonde screeningRonde, MammaCapaciteitBlok capaciteitBlok, Date vanaf, MammaStandplaatsPeriode standplaatsPeriode,
		MammaVerzettenReden verzettenReden, boolean vorigeAfspraakVerzetten, boolean notificeerBetrokkenSe, boolean isBulk, boolean stuurBerichtNaarSectra, boolean logGebeurtenis,
		Account account, boolean isGeforceerdeAfspraak, SmsStatus smsStatus)
	{
		var laatsteUitnodiging = screeningRonde.getLaatsteUitnodiging();

		var laatsteAfspraak = laatsteUitnodiging.getLaatsteAfspraak();
		if (laatsteAfspraak != null)
		{

			afspraakAnnuleren(laatsteAfspraak, MammaAfspraakStatus.VERPLAATST, null, vorigeAfspraakVerzetten, false);
		}

		if (screeningRonde.getLaatsteUitstel() != null)
		{
			uitstelService.uitstelAfzeggen(screeningRonde.getLaatsteUitstel(), MammaUitstelGeannuleerdReden.NIEUWE_AFSPRAAK, currentDateSupplier.getDate());
		}

		baseBriefService.setNietGegenereerdeBrievenOpTegenhouden(screeningRonde, BriefType.MAMMA_OPEN_UITNODIGINGEN);

		var afspraak = baseFactory.maakAfspraak(screeningRonde, capaciteitBlok, vanaf, standplaatsPeriode, verzettenReden, notificeerBetrokkenSe, stuurBerichtNaarSectra,
			isGeforceerdeAfspraak, smsStatus);

		if (logGebeurtenis)
		{
			var melding = getSaveAfspraakMelding(laatsteAfspraak, vanaf, standplaatsPeriode, isBulk, isGeforceerdeAfspraak);
			logService.logGebeurtenis(isGeforceerdeAfspraak ? LogGebeurtenis.MAMMA_AFSPRAAK_GEFORCEERD : LogGebeurtenis.MAMMA_AFSPRAAK_VERZET, account,
				screeningRonde.getDossier().getClient(), melding, Bevolkingsonderzoek.MAMMA);
		}

		kansberekeningService.dossierEventHerzien(screeningRonde.getDossier());

		return afspraak;
	}

	private String getSaveAfspraakMelding(MammaAfspraak huidigeAfspraak, Date vanaf, MammaStandplaatsPeriode standplaatsPeriode, boolean isBulk, boolean isGeforceerdeAfspraak)
	{
		String melding;
		if (huidigeAfspraak == null)
		{
			melding = String.format("Aangemaakt op %1$s in %2$s met %3$s", Constants.getDateTimeFormat().format(vanaf),
				standplaatsPeriode.getStandplaatsRonde().getStandplaats().getNaam(),
				standplaatsPeriode.getScreeningsEenheid().getNaam());
		}
		else
		{
			melding = String.format("Verzet van %1$s in %2$s naar %3$s in %4$s met %5$s", Constants.getDateTimeFormat().format(huidigeAfspraak.getVanaf()),
				huidigeAfspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats().getNaam(), Constants.getDateTimeFormat().format(vanaf),
				standplaatsPeriode.getStandplaatsRonde().getStandplaats().getNaam(),
				standplaatsPeriode.getScreeningsEenheid().getNaam());
			if (isBulk)
			{
				melding += " (bulk)";
			}
		}
		if (isGeforceerdeAfspraak)
		{
			melding += " (geforceerd)";
		}
		return melding;
	}

	@Override
	public BigDecimal getBenodigdeCapaciteit(List<MammaAfspraak> afspraken)
	{
		return afspraken.stream().map(MammaAfspraak::getBenodigdeCapaciteit).reduce(BigDecimal.ZERO, BigDecimal::add);
	}

	@Override
	public boolean heeftAfspraken(long standplaatsPeriodeId, MammaAfspraakStatus... afspraakStatussen)
	{
		return baseAfspraakRepository.exists(filterStatuses(Arrays.asList(afspraakStatussen))
			.and(heeftStandplaatsPeriode(standplaatsPeriodeId)));
	}

	@Override
	public long countAfspraken(MammaScreeningsEenheid screeningsEenheid, LocalDate vanaf, LocalDate totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		return baseAfspraakRepository.count(filterStatuses(Arrays.asList(afspraakStatussen))
			.and(begintTussenTotEnMet(vanaf, totEnMet))
			.and(heeftScreeningsEenheid(screeningsEenheid)));
	}

	@Override
	public Pair<Date, Date> getEersteEnLaatsteAfspraakMomenten(long standplaatsPeriodeId, LocalDate vanaf, LocalDate totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		var result = baseAfspraakRepository.findWith(filterStatuses(Arrays.asList(afspraakStatussen))
					.and(begintTussenTotEnMet(vanaf, totEnMet))
					.and(heeftStandplaatsPeriode(standplaatsPeriodeId)),
				Object[].class,
				q -> q.projections((cb, r) -> List.of(cb.least(r.get(MammaAfspraak_.vanaf)), cb.greatest(r.get(MammaAfspraak_.vanaf))))
					.one()
			)
			.orElseThrow();

		return Pair.of((Date) result[0], (Date) result[1]);
	}

	@Override
	public long countAfspraken(MammaStandplaats standplaats, LocalDate vanaf, LocalDate totEnMet, MammaAfspraakStatus... afspraakStatussen)
	{
		return baseAfspraakRepository.count(filterStatuses(Arrays.asList(afspraakStatussen))
			.and(begintTussenTotEnMet(vanaf, totEnMet))
			.and(heeftStandplaats(standplaats)));
	}

	@Override
	public List<MammaAfspraak> getAfspraken(MammaStandplaats standplaats, Range<Date> periode, MammaAfspraakStatus... afspraakStatussen)
	{
		return baseAfspraakRepository.findAll(filterStatuses(Arrays.asList(afspraakStatussen))
			.and(bevat(periode, r -> r.get(MammaAfspraak_.vanaf)))
			.and(heeftStandplaats(standplaats)));
	}

	@Transactional
	@Override
	public int koppelNietGekoppeldeAfspraken(MammaCapaciteitBlok capaciteitBlok, boolean runDry)
	{
		int aantalAfspraken = 0;
		if (capaciteitBlok.getBlokType() != MammaCapaciteitBlokType.GEEN_SCREENING)
		{
			LOG.debug("Zoek afspraken voor cap.blok om te kunnen (her)koppelen");
			var afspraken = getNietGekoppeldeAfspraken(capaciteitBlok);
			aantalAfspraken = afspraken.size();
			if (!runDry)
			{
				for (var afspraak : afspraken)
				{
					LOG.info("Afspraak van " + Constants.getDateTimeFormat().format(afspraak.getVanaf()) + " + voor client met id "
						+ afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient().getId() + " gekoppeld aan cap.blok");
					capaciteitBlok.getAfspraken().add(afspraak);
					afspraak.setCapaciteitBlok(capaciteitBlok);
				}
				capaciteitBlokRepository.save(capaciteitBlok);
			}
		}
		return aantalAfspraken;
	}

	private List<MammaAfspraak> getNietGekoppeldeAfspraken(MammaCapaciteitBlok capaciteitBlok)
	{
		var specification = heeftStatuses(List.of(MammaAfspraakStatus.GEPLAND))
			.and(begintTussen(DateUtil.toLocalDateTime(capaciteitBlok.getVanaf()), DateUtil.toLocalDateTime(capaciteitBlok.getTot())))
			.and(heeftGeenCapaciteitBlok())
			.and(heeftScreeningsEenheid(capaciteitBlok.getScreeningsEenheid()));

		var blokType = capaciteitBlok.getBlokType();
		if (blokType.equals(MammaCapaciteitBlokType.TEHUIS))
		{
			specification = specification.and(heeftClientInTehuis());
		}
		else
		{
			specification = specification.and(heeftGeenClientInTehuis()).and(heeftDoelgroep(blokType.getDoelgroepen()));
		}

		return baseAfspraakRepository.findAll(specification);
	}

	@Override
	public void afspraakAnnuleren(MammaAfspraak afspraak, MammaAfspraakStatus nieuweStatus, Date rondeAfgemeldOp)
	{
		boolean afspraakStatusWijzigen = afspraak.getVanaf().compareTo(currentDateSupplier.getDate()) > 0;
		afspraakAnnuleren(afspraak, nieuweStatus, rondeAfgemeldOp, afspraakStatusWijzigen, true);
	}

	@Override
	public void afspraakAnnuleren(MammaAfspraak afspraak, MammaAfspraakStatus nieuweStatus, Date rondeAfgemeldOp, boolean afspraakStatusWijzigen,
		boolean notificeerScreeningsEenhedenVerversenDaglijst)
	{
		var screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
		var origineleStatus = afspraak.getStatus();
		if (origineleStatus == MammaAfspraakStatus.GEPLAND && nieuweStatus != null
			&& afspraakStatusWijzigen) 
		{
			afspraak.setStatus(nieuweStatus);
			afspraak.setAfgezegdOp(rondeAfgemeldOp);

			var capaciteitBlok = afspraak.getCapaciteitBlok();
			if (capaciteitBlok != null)
			{
				capaciteitBlok.getAfspraken().remove(afspraak);
				afspraak.setCapaciteitBlok(null);
				hibernateService.saveOrUpdate(capaciteitBlok);
			}
			baseBriefService.setNietGegenereerdeBrievenOpTegenhouden(screeningRonde, Collections.singletonList(BriefType.MAMMA_AFSPRAAK_VERZET));
			hibernateService.saveOrUpdate(afspraak);

			berichtToBatchService.queueMammaHL7v24BerichtUitgaand(screeningRonde.getDossier().getClient(), MammaHL7v24ORMBerichtStatus.CANCELLED);
		}

		if (notificeerScreeningsEenhedenVerversenDaglijst && origineleStatus == MammaAfspraakStatus.GEPLAND)
		{
			berichtToSeRestBkService.notificeerScreeningsEenhedenVerversenDaglijst(screeningRonde.getDossier().getClient());
		}
	}

	@Override
	public MammaStandplaatsLocatie getMammaStandplaatsLocatieAfspraak(MammaAfspraak afspraak)
	{
		MammaStandplaatsLocatie locatie = null;
		if (afspraak != null)
		{
			var standplaats = afspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats();
			var datumAfspraak = DateUtil.toUtilDateMidnight(afspraak.getVanaf());
			locatie = standplaatsService.getStandplaatsLocatie(standplaats, datumAfspraak);
		}
		return locatie;
	}

	@Override
	public MammaStandplaatsLocatie getMammaStandplaatsLocatieUitnodiging(MammaUitnodiging uitnodiging)
	{
		MammaStandplaatsLocatie locatie = null;
		if (uitnodiging != null)
		{
			var afspraak = uitnodiging.getLaatsteAfspraak();
			if (afspraak != null)
			{
				var standplaats = afspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats();
				locatie = standplaatsService.getStandplaatsLocatie(standplaats, afspraak.getVanaf());
			}
			else if (uitnodiging.getStandplaatsRonde() != null)
			{
				var standplaats = uitnodiging.getStandplaatsRonde().getStandplaats();
				locatie = standplaatsService.getStandplaatsLocatie(standplaats, currentDateSupplier.getDate());
			}
		}
		return locatie;
	}

	@Override
	public MammaAfspraak getLaatsteAfspraakVanBriefronde(Brief brief)
	{
		brief = (Brief) HibernateHelper.deproxy(brief);
		if (brief instanceof MammaBrief)
		{
			var screeningRonde = ((MammaBrief) brief).getScreeningRonde();
			if (screeningRonde != null && screeningRonde.getLaatsteUitnodiging() != null)
			{
				return screeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak();
			}
		}
		return null;
	}

	@Override
	public MammaUitnodiging getLaatsteUitnodigingVanScreeningRonde(MammaScreeningRonde ronde)
	{
		if (ronde != null)
		{
			return ronde.getLaatsteUitnodiging();
		}
		return null;
	}

	@Override
	public boolean isAfspraakBinnen180Dagen(MammaOnderzoek onderzoek)
	{
		var minimaleIntervalMammografieOnderzoeken = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN.name());
		var minimaalIntervalOnderzoeken = DateUtil.toLocalDate(onderzoek.getMammografie().getAfgerondOp()).plusDays(minimaleIntervalMammografieOnderzoeken);
		return minimaalIntervalOnderzoeken.isAfter(currentDateSupplier.getLocalDate());
	}

	@Override
	public boolean isNoShow(MammaAfspraakStatus afspraakStatus, LocalDateTime afspraakMoment)
	{
		if (afspraakStatus == null || afspraakMoment == null)
		{
			return false;
		}
		var nu = currentDateSupplier.getLocalDateTime();
		return afspraakStatus == MammaAfspraakStatus.GEPLAND && nu.isAfter(afspraakMoment);
	}

	@Override
	public boolean briefKanNogVerzondenWorden(Date afspraakDatum)
	{
		var aantalWerkdagenBriefNietVersturenParameter = preferenceService.getInteger(PreferenceKey.MAMMA_BEVESTIGINGSBRIEF_NIET_VERZENDEN_BINNEN_AANTAL_WERKDAGEN.name());
		var minimumAfspraakDatum = DateUtil.plusWerkdagen(currentDateSupplier.getLocalDate(), aantalWerkdagenBriefNietVersturenParameter);
		return !DateUtil.toUtilDate(minimumAfspraakDatum).after(afspraakDatum);
	}

	@Override
	public boolean smsKanNogVerzondenWorden(LocalDateTime afspraakMoment)
	{
		var afspraakSmsHerinneringTermijn = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_SMS_HERINNERING_TERMIJN.name());
		var minimaalAfspraakMoment = currentDateSupplier.getLocalDateTime().plusHours(afspraakSmsHerinneringTermijn);
		return !minimaalAfspraakMoment.isAfter(afspraakMoment);
	}

	@Override
	public LocalDate getMinimaleAfspraakDatumBijUitnodigen()
	{
		var huidigeDagVoorPlannenAfspraken = createKandidaatAfsprakenDeterminatiePeriodeService().getHuidigeDagVoorPlannenAfspraken();
		var afspraakBijUitnodigenVanafAantalWerkdagen = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_BIJ_UITNODIGEN_VANAF_AANTAL_WERKDAGEN.toString());
		return DateUtil.plusWerkdagen(huidigeDagVoorPlannenAfspraken, afspraakBijUitnodigenVanafAantalWerkdagen);
	}

	@Override
	public boolean magUitstellen(MammaDossier dossier)
	{
		return magUitstellen(dossier, false);
	}

	@Override
	public boolean magUitstellen(MammaDossier dossier, boolean bijAfspraakForceren)
	{
		var laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();

		var laatsteAfspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(laatsteScreeningRonde);

		boolean isLaatsteAfspraakGeenGeforceerdeAfspraak = laatsteAfspraak == null || !laatsteAfspraak.isGeforceerdeAfspraak();

		boolean heeftRondeGeenOnderzoek = laatsteScreeningRonde == null || laatsteScreeningRonde.getLaatsteOnderzoek() == null;

		boolean isGeenTehuisClient = dossier.getTehuis() == null;

		return heeftRondeGeenOnderzoek && isLaatsteAfspraakGeenGeforceerdeAfspraak && isGeenTehuisClient && !bijAfspraakForceren;
	}

	private static @Nullable Specification<MammaAfspraak> begintTussenTotEnMet(LocalDate vanaf, LocalDate totEnMet)
	{
		return begintTussen(vanaf != null ? vanaf.atStartOfDay() : null, totEnMet != null ? totEnMet.plusDays(1).atStartOfDay() : null);
	}
}
