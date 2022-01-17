package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.NavigableMap;

import javax.annotation.PostConstruct;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.mamma.MammaBaseStandplaatsDao;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Uitnodiging;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaKansberekeningAfspraakEvent;
import nl.rivm.screenit.model.mamma.MammaKansberekeningEvent;
import nl.rivm.screenit.model.mamma.MammaKansberekeningRegioGemiddelden;
import nl.rivm.screenit.model.mamma.MammaKansberekeningScreeningRondeEvent;
import nl.rivm.screenit.model.mamma.MammaKansberekeningStandplaatsRondeGemiddelden;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaRegioType;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.rest.RestApiFactory;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseKansberekeningServiceImpl implements MammaBaseKansberekeningService
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaBaseKansberekeningServiceImpl.class);

	private static final BigDecimal MINIMUM_OPKOMSTKANS = new BigDecimal(0.01);

	private static final int KANSBEREKENING_TIMEOUT_MS = 5000;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaBaseStandplaatsDao standplaatsDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	@Lazy
	private MammaBaseDossierService dossierService;

	@Autowired
	@Qualifier("kansberekeningServiceUrl")
	private String kansberekeningServiceUrl;

	private Integer minimaleLeeftijd;

	private Boolean useKansbereking;

	private Integer defaultOpkomstkansZonderKansberekening;

	@PostConstruct
	private void init()
	{
		if (StringUtils.isNotBlank(kansberekeningServiceUrl) && !kansberekeningServiceUrl.endsWith("/"))
		{
			kansberekeningServiceUrl += "/";
		}
	}

	@Override
	public void updateScreeningRondeEvent(MammaDossier dossier, boolean zetDeelname)
	{
		updateScreeningRondeEvent(new MammaKansberekeningScreeningRondeContext(dossier), dossier.getScreeningRondeEvent(), zetDeelname);
	}

	@Override
	public void updateScreeningRondeEvent(MammaScreeningRonde screeningRonde, boolean zetDeelname)
	{
		updateScreeningRondeEvent(new MammaKansberekeningScreeningRondeContext(screeningRonde), screeningRonde.getScreeningRondeEvent(), zetDeelname);
	}

	private void updateScreeningRondeEvent(MammaKansberekeningScreeningRondeContext context, MammaKansberekeningScreeningRondeEvent screeningRondeEvent,
		boolean zetDeelname)
	{
		if (zetDeelname)
		{
			screeningRondeEvent.setDeelname(context.screeningRondeMetOnderzoekSet.contains(screeningRondeEvent.getScreeningRonde()));
		}

		if (context.screeningRonde != null)
		{

			MammaUitnodiging eersteUitnodiging = context.screeningRonde.getUitnodigingen().stream().min(Comparator.comparing(Uitnodiging::getCreatieDatum)).orElse(null);
			screeningRondeEvent.setPeilEpochDay(context.screeningRondeCreatieDatum.toEpochDay());
			screeningRondeEvent.setSuspect(BriefType.MAMMA_UITNODIGING_SUSPECT.equals(eersteUitnodiging.getBrief().getBriefType()));
		}
		else
		{
			screeningRondeEvent.setPeilEpochDay(context.geboorteDatum.plusYears(getMinimaleLeeftijd()).toEpochDay());
			screeningRondeEvent.setSuspect(dossierService.isSuspect(context));
		}

		updateKansberekeningenEvent(screeningRondeEvent, context, context.screeningRondeCreatieDatum != null ? context.screeningRondeCreatieDatum : dateSupplier.getLocalDate());
	}

	@Override
	public void updateAfspraakEvent(MammaAfspraak afspraak, boolean zetOpkomst)
	{
		MammaKansberekeningAfspraakEvent afspraakEvent = afspraak.getAfspraakEvent();
		MammaKansberekeningAfspraakContext context = new MammaKansberekeningAfspraakContext(afspraak);
		if (zetOpkomst)
		{
			afspraakEvent.setOpkomst(context.afspraakMetOnderzoekSet.contains(afspraakEvent.getAfspraak()));
		}

		afspraakEvent.setPeilEpochDay(DateUtil.toLocalDate(afspraak.getVanaf()).toEpochDay());
		afspraakEvent.setMaand(context.maand);
		afspraakEvent.setUur(context.uur);
		afspraakEvent.setVerzettenReden(context.verzettenReden);
		afspraakEvent.setBriefTypeUitnodiging(context.briefTypeUitnodiging);
		afspraakEvent.setNaHerinnering(context.uitnodiging.getHerinnered());
		MammaAfmelding laatsteAfmelding = context.screeningRonde.getLaatsteAfmelding();
		afspraakEvent.setNaHeraanmelding(laatsteAfmelding != null && laatsteAfmelding.getHeraanmeldStatus() == AanvraagBriefStatus.VERWERKT);
		afspraakEvent.setRondeGeforceerd(context.screeningRonde.getIsGeforceerd());

		updateKansberekeningenEvent(afspraakEvent, context, context.vanaf != null ? context.vanaf.toLocalDate() : dateSupplier.getLocalDate());
	}

	private void updateKansberekeningenEvent(MammaKansberekeningEvent kansberekeningEvent, MammaKansberekeningScreeningRondeContext context, LocalDate peildatum)
	{
		kansberekeningEvent.setWijzigingsDatum(dateSupplier.getDate());

		long leeftijd = ChronoUnit.YEARS.between(context.geboorteDatum, peildatum);
		kansberekeningEvent.setLeeftijd(leeftijd);
		kansberekeningEvent.setLeeftijdPer5(leeftijd - leeftijd % 5);
		kansberekeningEvent.setDoelgroep(context.dossier.getDoelgroep());
		kansberekeningEvent.setTehuis(context.dossier.getTehuis() != null);

		NavigableMap<LocalDate, MammaScreeningRonde> vorigeScreeningRondeNavigableMap = context.screeningRondeNavigableMap.headMap(peildatum, false);
		NavigableMap<LocalDate, MammaUitnodiging> vorigeUitnodigingenNavigableMap = context.uitnodigingNavigableMap.headMap(peildatum, false);
		NavigableMap<LocalDate, MammaAfspraak> vorigeAfspraakNavigableMap = context.afspraakNavigableMap.headMap(peildatum, false);
		NavigableMap<LocalDate, MammaBeoordeling> vorigeLaatsteBeoordelingNavigableMap = context.laatsteBeoordelingNavigableMap.headMap(peildatum, false);

		if (context.screeningRonde != null)
		{
			vorigeScreeningRondeNavigableMap.remove(DateUtil.toLocalDate(context.screeningRonde.getCreatieDatum()));
		}
		if (context.uitnodiging != null)
		{
			vorigeUitnodigingenNavigableMap.remove(DateUtil.toLocalDate(context.uitnodiging.getCreatieDatum()));
		}

		kansberekeningEvent.setVoorgaandeScreeningRondes(vorigeScreeningRondeNavigableMap.size());
		kansberekeningEvent.setVoorgaandeUitnodigingen(vorigeUitnodigingenNavigableMap.size());
		kansberekeningEvent.setVoorgaandeAfspraken(vorigeAfspraakNavigableMap.size());
		kansberekeningEvent.setVoorgaandeOnderzoeken(context.onderzoekNavigableMap.headMap(peildatum, false).size());
		kansberekeningEvent.setVoorgaandeMammogrammen(context.mammografieNavigableMap.headMap(peildatum, false).size());

		if (!vorigeScreeningRondeNavigableMap.isEmpty())
		{
			MammaScreeningRonde vorigeScreeningRonde = vorigeScreeningRondeNavigableMap.lastEntry().getValue();

			kansberekeningEvent.setAfgemeldVorigeScreeningRonde(vorigeScreeningRonde == null ? null : !vorigeScreeningRonde.getAangemeld());
			kansberekeningEvent.setDeelnameVorigeScreeningRonde(context.screeningRondeMetOnderzoekSet.contains(vorigeScreeningRonde));

			vorigeScreeningRondeNavigableMap = vorigeScreeningRondeNavigableMap.tailMap(peildatum.minusYears(10), true);
			context.screeningRondeMetOnderzoekSet.retainAll(vorigeScreeningRondeNavigableMap.values());
			kansberekeningEvent.setHistorieScoreScreeningRonde10Jaar(bepaalHistorieScore(vorigeScreeningRondeNavigableMap.size(), context.screeningRondeMetOnderzoekSet.size()));

			vorigeScreeningRondeNavigableMap = vorigeScreeningRondeNavigableMap.tailMap(peildatum.minusYears(5), true);
			context.screeningRondeMetOnderzoekSet.retainAll(vorigeScreeningRondeNavigableMap.values());
			kansberekeningEvent.setHistorieScoreScreeningRonde5Jaar(bepaalHistorieScore(vorigeScreeningRondeNavigableMap.size(), context.screeningRondeMetOnderzoekSet.size()));

			vorigeScreeningRondeNavigableMap = vorigeScreeningRondeNavigableMap.tailMap(peildatum.minusYears(3), true);
			context.screeningRondeMetOnderzoekSet.retainAll(vorigeScreeningRondeNavigableMap.values());
			kansberekeningEvent.setHistorieScoreScreeningRonde3Jaar(bepaalHistorieScore(vorigeScreeningRondeNavigableMap.size(), context.screeningRondeMetOnderzoekSet.size()));
		}

		if (!vorigeAfspraakNavigableMap.isEmpty())
		{
			MammaAfspraak vorigeAfspraak = vorigeAfspraakNavigableMap.lastEntry().getValue();
			kansberekeningEvent.setOpkomstVorigeAfspraak(context.afspraakMetOnderzoekSet.contains(vorigeAfspraak));
			kansberekeningEvent.setAfspraakStatusVorigeAfspraak(vorigeAfspraak.getStatus());

			vorigeAfspraakNavigableMap = vorigeAfspraakNavigableMap.tailMap(peildatum.minusYears(10), true);
			context.afspraakMetOnderzoekSet.retainAll(vorigeAfspraakNavigableMap.values());
			kansberekeningEvent.setHistorieScoreAfspraak10Jaar(bepaalHistorieScore(vorigeAfspraakNavigableMap.size(), context.afspraakMetOnderzoekSet.size()));

			vorigeAfspraakNavigableMap = vorigeAfspraakNavigableMap.tailMap(peildatum.minusYears(5), true);
			context.afspraakMetOnderzoekSet.retainAll(vorigeAfspraakNavigableMap.values());
			kansberekeningEvent.setHistorieScoreAfspraak5Jaar(bepaalHistorieScore(vorigeAfspraakNavigableMap.size(), context.afspraakMetOnderzoekSet.size()));

			vorigeAfspraakNavigableMap = vorigeAfspraakNavigableMap.tailMap(peildatum.minusYears(3), true);
			context.afspraakMetOnderzoekSet.retainAll(vorigeAfspraakNavigableMap.values());
			kansberekeningEvent.setHistorieScoreAfspraak3Jaar(bepaalHistorieScore(vorigeAfspraakNavigableMap.size(), context.afspraakMetOnderzoekSet.size()));
		}

		if (!vorigeLaatsteBeoordelingNavigableMap.isEmpty())
		{
			MammaBeoordeling vorigeBeoordeling = vorigeLaatsteBeoordelingNavigableMap.lastEntry().getValue();
			kansberekeningEvent.setBeoordelingStatusVorigeBeoordeling(vorigeBeoordeling.getStatus());
		}
	}

	private BigDecimal bepaalHistorieScore(long aantalRondes, long aantalDeelnamen)
	{
		return new BigDecimal(aantalDeelnamen).add(new BigDecimal(1)).divide(new BigDecimal(aantalRondes).add(new BigDecimal(2)), 5, RoundingMode.HALF_UP);
	}

	@Override
	public void fitDossierClassifier()
	{
		RestTemplate restTemplate = RestApiFactory.create();
		restTemplate.getForObject(kansberekeningServiceUrl + "fit_dossier_classifier", String.class);
	}

	@Override
	public void fitAfsprakenClassifier()
	{
		RestTemplate restTemplate = RestApiFactory.create();
		restTemplate.getForObject(kansberekeningServiceUrl + "fit_afspraak_classifier", String.class);
	}

	@Override
	public void predictDossiers()
	{
		RestTemplate restTemplate = RestApiFactory.create();
		restTemplate.getForObject(kansberekeningServiceUrl + "predict_dossiers", String.class);
	}

	@Override
	public void predictAfspraken()
	{
		RestTemplate restTemplate = RestApiFactory.create();
		restTemplate.getForObject(kansberekeningServiceUrl + "predict_afspraken", String.class);
	}

	@Override
	public BigDecimal getOpkomstkans(MammaAfspraak afspraak)
	{
		if (mayUseKansberekening())
		{
			RestTemplate restTemplate = RestApiFactory.create(KANSBEREKENING_TIMEOUT_MS);

			try
			{
				BigDecimal opkomstkans = new BigDecimal(
					restTemplate.postForObject(kansberekeningServiceUrl + "predict_afspraak", "[" + new AfspraakEventDto(afspraak).toString() + "]", String.class));
				return opkomstkans.compareTo(MINIMUM_OPKOMSTKANS) >= 0 ? opkomstkans : MINIMUM_OPKOMSTKANS;
			}
			catch (RuntimeException ex)
			{
				LOG.error("Kon geen kans berekenen voor afspraakID: '{}'", afspraak.getId(), ex);
				return BigDecimal.ONE;
			}
		}
		else
		{
			return BigDecimal.valueOf(getDefaultOpkomstkansZonderKansberekening()).divide(BigDecimal.valueOf(100), 2, RoundingMode.HALF_UP);
		}
	}

	private Boolean mayUseKansberekening()
	{
		if (useKansbereking == null)
		{
			resetPreferences();
		}
		return useKansbereking;
	}

	private Integer getDefaultOpkomstkansZonderKansberekening()
	{
		if (defaultOpkomstkansZonderKansberekening == null)
		{
			resetPreferences();
		}
		return defaultOpkomstkansZonderKansberekening;
	}

	public Integer getMinimaleLeeftijd()
	{
		if (minimaleLeeftijd == null)
		{
			resetPreferences();
		}
		return minimaleLeeftijd;
	}

	@Override
	public BigDecimal getVoorlopigeOpkomstkans(MammaDossier dossier, MammaStandplaatsPeriode standplaatsPeriode, MammaVerzettenReden verzettenReden,
		BriefType briefTypeUitnodiging)
	{
		MammaBrief dummyBrief = new MammaBrief();
		dummyBrief.setBriefType(briefTypeUitnodiging);

		MammaUitnodiging dummyUitnodiging = new MammaUitnodiging();
		dummyUitnodiging.setCreatieDatum(dateSupplier.getDate());
		dummyUitnodiging.setHerinnered(false);
		dummyUitnodiging.setBrief(dummyBrief);

		MammaScreeningRonde dummyScreeningRonde = new MammaScreeningRonde();
		dummyScreeningRonde.setCreatieDatum(dateSupplier.getDate());
		dummyUitnodiging.setScreeningRonde(dummyScreeningRonde);

		dossier.getScreeningRondes().add(dummyScreeningRonde);
		dummyScreeningRonde.setDossier(dossier);

		BigDecimal voorlopigeOpkomstkans = getVoorlopigeOpkomstkans(dummyUitnodiging, standplaatsPeriode, verzettenReden);
		dossier.getScreeningRondes().remove(dummyScreeningRonde);
		return voorlopigeOpkomstkans;
	}

	@Override
	public BigDecimal getVoorlopigeOpkomstkans(MammaUitnodiging uitnodiging, MammaStandplaatsPeriode standplaatsPeriode, MammaVerzettenReden verzettenReden)
	{
		MammaAfspraak dummyAfspraak = new MammaAfspraak();
		dummyAfspraak.setStandplaatsPeriode(standplaatsPeriode);
		dummyAfspraak.setVanaf(dateSupplier.getDate());
		dummyAfspraak.setStatus(MammaAfspraakStatus.GEPLAND);

		GbaPersoon gbaPersoon = uitnodiging.getScreeningRonde().getDossier().getClient().getPersoon();
		dummyAfspraak
			.setPostcode(gbaPersoon.getTijdelijkGbaAdres() != null && gbaPersoon.getTijdelijkGbaAdres().getPostcode() != null ? gbaPersoon.getTijdelijkGbaAdres().getPostcode()
				: gbaPersoon.getGbaAdres().getPostcode());
		dummyAfspraak.setVerzettenReden(verzettenReden);
		dummyAfspraak.setAfspraakEvent(new MammaKansberekeningAfspraakEvent());
		dummyAfspraak.setUitnodiging(uitnodiging);

		resetPreferences();
		return getOpkomstkans(dummyAfspraak);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void maakDossierEvent(MammaDossier dossier)
	{
		getMinimaleLeeftijd(); 
		MammaKansberekeningScreeningRondeEvent screeningRondeEvent = dossier.getScreeningRondeEvent();
		if (screeningRondeEvent == null)
		{
			screeningRondeEvent = new MammaKansberekeningScreeningRondeEvent();
			screeningRondeEvent.setDossier(dossier);
			dossier.setScreeningRondeEvent(screeningRondeEvent);

			hibernateService.saveOrUpdate(dossier);
		}

		updateScreeningRondeEvent(dossier, false);

		hibernateService.saveOrUpdate(screeningRondeEvent);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void dossierEventHerzien(MammaDossier dossier)
	{
		MammaKansberekeningScreeningRondeEvent screeningRondeEvent = dossier.getScreeningRondeEvent();

		if (screeningRondeEvent != null)
		{
			updateScreeningRondeEvent(dossier, false);
			hibernateService.saveOrUpdate(screeningRondeEvent);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void screeningRondeSampleHerzien(MammaScreeningRonde screeningRonde)
	{
		MammaKansberekeningScreeningRondeEvent screeningRondeEvent = screeningRonde.getScreeningRondeEvent();
		if (screeningRondeEvent != null)
		{
			updateScreeningRondeEvent(screeningRonde, true);
			hibernateService.saveOrUpdate(screeningRondeEvent);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void kansberekeningHerzien(MammaDossier dossier, LocalDate vanaf)
	{
		MammaKansberekeningScreeningRondeEvent dossierEvent = dossier.getScreeningRondeEvent();
		if (dossierEvent != null && DateUtil.toLocalDate(dossierEvent.getWijzigingsDatum()).isAfter(vanaf))
		{
			dossierEventHerzien(dossier);
		}

		for (MammaScreeningRonde screeningRonde : dossier.getScreeningRondes())
		{
			MammaKansberekeningScreeningRondeEvent screeningRondeEvent = screeningRonde.getScreeningRondeEvent();
			if (screeningRondeEvent != null && DateUtil.toLocalDate(screeningRondeEvent.getWijzigingsDatum()).isAfter(vanaf))
			{
				screeningRondeSampleHerzien(screeningRonde);
			}

			for (MammaUitnodiging uitnodiging : screeningRonde.getUitnodigingen())
			{
				for (MammaAfspraak afspraak : uitnodiging.getAfspraken())
				{
					MammaKansberekeningAfspraakEvent afspraakEvent = afspraak.getAfspraakEvent();
					if (afspraakEvent != null && DateUtil.toLocalDate(afspraakEvent.getWijzigingsDatum()).isAfter(vanaf))
					{
						updateAfspraakEvent(afspraak, false);
					}
				}
			}
		}
	}

	private class AfspraakEventDto implements Serializable
	{
		private BigDecimal landelijk_gem_deelname_afgelopen10jaar;

		private BigDecimal landelijk_gem_deelname_afgelopen3jaar;

		private BigDecimal landelijk_gem_deelname_afgelopen5jaar;

		private BigDecimal landelijk_gem_deelname_eerste_ronde_afgelopen10jaar;

		private BigDecimal landelijk_gem_deelname_eerste_ronde_afgelopen3jaar;

		private BigDecimal landelijk_gem_deelname_eerste_ronde_afgelopen5jaar;

		private BigDecimal landelijk_gem_opkomst_afgelopen10jaar;

		private BigDecimal landelijk_gem_opkomst_afgelopen3jaar;

		private BigDecimal landelijk_gem_opkomst_afgelopen5jaar;

		private BigDecimal landelijk_gem_opkomst_eerste_ronde_afgelopen10jaar;

		private BigDecimal landelijk_gem_opkomst_eerste_ronde_afgelopen3jaar;

		private BigDecimal landelijk_gem_opkomst_eerste_ronde_afgelopen5jaar;

		private BigDecimal postcode_cijfers_gem_deelname_afgelopen10jaar;

		private BigDecimal postcode_cijfers_gem_deelname_afgelopen3jaar;

		private BigDecimal postcode_cijfers_gem_deelname_afgelopen5jaar;

		private BigDecimal postcode_cijfers_gem_deelname_eerste_ronde_afgelopen10jaar;

		private BigDecimal postcode_cijfers_gem_deelname_eerste_ronde_afgelopen3jaar;

		private BigDecimal postcode_cijfers_gem_deelname_eerste_ronde_afgelopen5jaar;

		private BigDecimal postcode_cijfers_gem_opkomst_afgelopen10jaar;

		private BigDecimal postcode_cijfers_gem_opkomst_afgelopen3jaar;

		private BigDecimal postcode_cijfers_gem_opkomst_afgelopen5jaar;

		private BigDecimal postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen10jaar;

		private BigDecimal postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen3jaar;

		private BigDecimal postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen5jaar;

		private BigDecimal postcode_gem_deelname_afgelopen10jaar;

		private BigDecimal postcode_gem_deelname_afgelopen3jaar;

		private BigDecimal postcode_gem_deelname_afgelopen5jaar;

		private BigDecimal postcode_gem_deelname_eerste_ronde_afgelopen10jaar;

		private BigDecimal postcode_gem_deelname_eerste_ronde_afgelopen3jaar;

		private BigDecimal postcode_gem_deelname_eerste_ronde_afgelopen5jaar;

		private BigDecimal postcode_gem_opkomst_afgelopen10jaar;

		private BigDecimal postcode_gem_opkomst_afgelopen3jaar;

		private BigDecimal postcode_gem_opkomst_afgelopen5jaar;

		private BigDecimal postcode_gem_opkomst_eerste_ronde_afgelopen10jaar;

		private BigDecimal postcode_gem_opkomst_eerste_ronde_afgelopen3jaar;

		private BigDecimal postcode_gem_opkomst_eerste_ronde_afgelopen5jaar;

		private BigDecimal huidige_standplaats_ronde_gem_deelname_eerste_ronde;

		private BigDecimal huidige_standplaats_ronde_gem_deelname;

		private BigDecimal huidige_standplaats_ronde_gem_opkomst_eerste_ronde;

		private BigDecimal huidige_standplaats_ronde_gem_opkomst;

		private BigDecimal vorige_standplaats_ronde_gem_deelname_eerste_ronde;

		private BigDecimal vorige_standplaats_ronde_gem_deelname;

		private BigDecimal vorige_standplaats_ronde_gem_opkomst_eerste_ronde;

		private BigDecimal vorige_standplaats_ronde_gem_opkomst;

		private Long event_peil_epoch_day;

		private Boolean event_afgemeld_vorige_screening_ronde;

		private MammaAfspraakStatus event_afspraak_status_vorige_afspraak;

		private MammaBeoordelingStatus event_beoordeling_status_vorige_beoordeling;

		private Boolean event_deelname_vorige_screening_ronde;

		private MammaDoelgroep event_doelgroep;

		private BigDecimal event_historie_score_afspraak10jaar;

		private BigDecimal event_historie_score_afspraak3jaar;

		private BigDecimal event_historie_score_afspraak5jaar;

		private BigDecimal event_historie_score_screening_ronde10jaar;

		private BigDecimal event_historie_score_screening_ronde3jaar;

		private BigDecimal event_historie_score_screening_ronde5jaar;

		private Long event_leeftijd;

		private Long event_leeftijd_per5;

		private Boolean event_opkomst_vorige_afspraak;

		private Boolean event_tehuis;

		private Integer event_voorgaande_afspraken;

		private Integer event_voorgaande_mammogrammen;

		private Integer event_voorgaande_onderzoeken;

		private Integer event_voorgaande_screening_rondes;

		private Integer event_voorgaande_uitnodigingen;

		private BriefType event_brief_type_uitnodiging;

		private Integer event_maand;

		private Boolean event_na_heraanmelding;

		private Boolean event_na_herinnering;

		private Boolean event_ronde_geforceerd;

		private Integer event_uur;

		private MammaVerzettenReden event_verzetten_reden;

		private AfspraakEventDto(MammaAfspraak afspraak)
		{
			String postcode = afspraak.getPostcode();
			String postcodeCijfers = postcode.substring(0, 4);

			Map<String, Object> parameters = new HashMap<>();

			parameters.put("regioType", MammaRegioType.LANDELIJK);
			MammaKansberekeningRegioGemiddelden landelijkeGemiddelden = hibernateService.getUniqueByParameters(MammaKansberekeningRegioGemiddelden.class, parameters);

			parameters.put("regioType", MammaRegioType.POSTCODE_CIJFERS);
			parameters.put("regio", postcodeCijfers);
			MammaKansberekeningRegioGemiddelden postcodeCijfersGemiddelden = hibernateService.getUniqueByParameters(MammaKansberekeningRegioGemiddelden.class, parameters);

			parameters.put("regioType", MammaRegioType.POSTCODE);
			parameters.put("regio", postcode);
			MammaKansberekeningRegioGemiddelden postcodeGemiddelden = hibernateService.getUniqueByParameters(MammaKansberekeningRegioGemiddelden.class, parameters);

			MammaStandplaatsRonde standplaatsRonde = afspraak.getStandplaatsPeriode().getStandplaatsRonde();
			MammaKansberekeningStandplaatsRondeGemiddelden huidigeStandplaatsRondeGemiddelden = standplaatsRonde.getStandplaatsRondeGemiddelden();
			if (huidigeStandplaatsRondeGemiddelden == null)
			{

				standplaatsRonde = standplaatsDao.getVorigeStandplaatsRonde(standplaatsRonde);
				huidigeStandplaatsRondeGemiddelden = standplaatsRonde != null && standplaatsRonde.getStandplaatsRondeGemiddelden() != null
					? standplaatsRonde.getStandplaatsRondeGemiddelden()
					: new MammaKansberekeningStandplaatsRondeGemiddelden();
			}

			MammaStandplaatsRonde vorigeStandplaatsRonde = standplaatsRonde != null ? standplaatsDao.getVorigeStandplaatsRonde(standplaatsRonde) : null;
			MammaKansberekeningStandplaatsRondeGemiddelden vorigeStandplaatsRondeGemiddelen = vorigeStandplaatsRonde != null
				&& vorigeStandplaatsRonde.getStandplaatsRondeGemiddelden() != null
					? vorigeStandplaatsRonde.getStandplaatsRondeGemiddelden()
					: new MammaKansberekeningStandplaatsRondeGemiddelden();

			updateAfspraakEvent(afspraak, false);
			MammaKansberekeningAfspraakEvent afspraakEvent = afspraak.getAfspraakEvent();

			landelijk_gem_deelname_afgelopen10jaar = landelijkeGemiddelden.getDeelnameAfgelopen10Jaar();
			landelijk_gem_deelname_afgelopen3jaar = landelijkeGemiddelden.getDeelnameAfgelopen3Jaar();
			landelijk_gem_deelname_afgelopen5jaar = landelijkeGemiddelden.getDeelnameAfgelopen5Jaar();
			landelijk_gem_deelname_eerste_ronde_afgelopen10jaar = landelijkeGemiddelden.getDeelnameEersteRondeAfgelopen10Jaar();
			landelijk_gem_deelname_eerste_ronde_afgelopen3jaar = landelijkeGemiddelden.getDeelnameEersteRondeAfgelopen3Jaar();
			landelijk_gem_deelname_eerste_ronde_afgelopen5jaar = landelijkeGemiddelden.getDeelnameEersteRondeAfgelopen5Jaar();
			landelijk_gem_opkomst_afgelopen10jaar = landelijkeGemiddelden.getOpkomstAfgelopen10Jaar();
			landelijk_gem_opkomst_afgelopen3jaar = landelijkeGemiddelden.getOpkomstAfgelopen3Jaar();
			landelijk_gem_opkomst_afgelopen5jaar = landelijkeGemiddelden.getOpkomstAfgelopen5Jaar();
			landelijk_gem_opkomst_eerste_ronde_afgelopen10jaar = landelijkeGemiddelden.getOpkomstEersteRondeAfgelopen10Jaar();
			landelijk_gem_opkomst_eerste_ronde_afgelopen3jaar = landelijkeGemiddelden.getOpkomstEersteRondeAfgelopen3Jaar();
			landelijk_gem_opkomst_eerste_ronde_afgelopen5jaar = landelijkeGemiddelden.getOpkomstEersteRondeAfgelopen5Jaar();
			if (postcodeCijfersGemiddelden != null)
			{
				postcode_cijfers_gem_deelname_afgelopen10jaar = postcodeCijfersGemiddelden.getDeelnameAfgelopen10Jaar();
				postcode_cijfers_gem_deelname_afgelopen3jaar = postcodeCijfersGemiddelden.getDeelnameAfgelopen3Jaar();
				postcode_cijfers_gem_deelname_afgelopen5jaar = postcodeCijfersGemiddelden.getDeelnameAfgelopen5Jaar();
				postcode_cijfers_gem_deelname_eerste_ronde_afgelopen10jaar = postcodeCijfersGemiddelden.getDeelnameEersteRondeAfgelopen10Jaar();
				postcode_cijfers_gem_deelname_eerste_ronde_afgelopen3jaar = postcodeCijfersGemiddelden.getDeelnameEersteRondeAfgelopen3Jaar();
				postcode_cijfers_gem_deelname_eerste_ronde_afgelopen5jaar = postcodeCijfersGemiddelden.getDeelnameEersteRondeAfgelopen5Jaar();
				postcode_cijfers_gem_opkomst_afgelopen10jaar = postcodeCijfersGemiddelden.getOpkomstAfgelopen10Jaar();
				postcode_cijfers_gem_opkomst_afgelopen3jaar = postcodeCijfersGemiddelden.getOpkomstAfgelopen3Jaar();
				postcode_cijfers_gem_opkomst_afgelopen5jaar = postcodeCijfersGemiddelden.getOpkomstAfgelopen5Jaar();
				postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen10jaar = postcodeCijfersGemiddelden.getOpkomstEersteRondeAfgelopen10Jaar();
				postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen3jaar = postcodeCijfersGemiddelden.getOpkomstEersteRondeAfgelopen3Jaar();
				postcode_cijfers_gem_opkomst_eerste_ronde_afgelopen5jaar = postcodeCijfersGemiddelden.getOpkomstEersteRondeAfgelopen5Jaar();
				if (postcodeGemiddelden != null)
				{
					postcode_gem_deelname_afgelopen10jaar = postcodeGemiddelden.getDeelnameAfgelopen10Jaar();
					postcode_gem_deelname_afgelopen3jaar = postcodeGemiddelden.getDeelnameAfgelopen3Jaar();
					postcode_gem_deelname_afgelopen5jaar = postcodeGemiddelden.getDeelnameAfgelopen5Jaar();
					postcode_gem_deelname_eerste_ronde_afgelopen10jaar = postcodeGemiddelden.getDeelnameEersteRondeAfgelopen10Jaar();
					postcode_gem_deelname_eerste_ronde_afgelopen3jaar = postcodeGemiddelden.getDeelnameEersteRondeAfgelopen3Jaar();
					postcode_gem_deelname_eerste_ronde_afgelopen5jaar = postcodeGemiddelden.getDeelnameEersteRondeAfgelopen5Jaar();
					postcode_gem_opkomst_afgelopen10jaar = postcodeGemiddelden.getOpkomstAfgelopen10Jaar();
					postcode_gem_opkomst_afgelopen3jaar = postcodeGemiddelden.getOpkomstAfgelopen3Jaar();
					postcode_gem_opkomst_afgelopen5jaar = postcodeGemiddelden.getOpkomstAfgelopen5Jaar();
					postcode_gem_opkomst_eerste_ronde_afgelopen10jaar = postcodeGemiddelden.getOpkomstEersteRondeAfgelopen10Jaar();
					postcode_gem_opkomst_eerste_ronde_afgelopen3jaar = postcodeGemiddelden.getOpkomstEersteRondeAfgelopen3Jaar();
					postcode_gem_opkomst_eerste_ronde_afgelopen5jaar = postcodeGemiddelden.getOpkomstEersteRondeAfgelopen5Jaar();
				}
			}

			huidige_standplaats_ronde_gem_deelname_eerste_ronde = huidigeStandplaatsRondeGemiddelden.getDeelnameEersteRonde();
			huidige_standplaats_ronde_gem_deelname = huidigeStandplaatsRondeGemiddelden.getDeelname();
			huidige_standplaats_ronde_gem_opkomst_eerste_ronde = huidigeStandplaatsRondeGemiddelden.getOpkomstEersteRonde();
			huidige_standplaats_ronde_gem_opkomst = huidigeStandplaatsRondeGemiddelden.getOpkomst();
			vorige_standplaats_ronde_gem_deelname_eerste_ronde = vorigeStandplaatsRondeGemiddelen.getDeelnameEersteRonde();
			vorige_standplaats_ronde_gem_deelname = vorigeStandplaatsRondeGemiddelen.getDeelname();
			vorige_standplaats_ronde_gem_opkomst_eerste_ronde = vorigeStandplaatsRondeGemiddelen.getOpkomstEersteRonde();
			vorige_standplaats_ronde_gem_opkomst = vorigeStandplaatsRondeGemiddelen.getOpkomst();
			event_peil_epoch_day = afspraakEvent.getPeilEpochDay();
			event_afgemeld_vorige_screening_ronde = afspraakEvent.getAfgemeldVorigeScreeningRonde();
			event_afspraak_status_vorige_afspraak = afspraakEvent.getAfspraakStatusVorigeAfspraak();
			event_beoordeling_status_vorige_beoordeling = afspraakEvent.getBeoordelingStatusVorigeBeoordeling();
			event_deelname_vorige_screening_ronde = afspraakEvent.getDeelnameVorigeScreeningRonde();
			event_doelgroep = afspraakEvent.getDoelgroep();
			event_historie_score_afspraak10jaar = afspraakEvent.getHistorieScoreAfspraak10Jaar();
			event_historie_score_afspraak3jaar = afspraakEvent.getHistorieScoreAfspraak3Jaar();
			event_historie_score_afspraak5jaar = afspraakEvent.getHistorieScoreAfspraak5Jaar();
			event_historie_score_screening_ronde10jaar = afspraakEvent.getHistorieScoreScreeningRonde10Jaar();
			event_historie_score_screening_ronde3jaar = afspraakEvent.getHistorieScoreScreeningRonde3Jaar();
			event_historie_score_screening_ronde5jaar = afspraakEvent.getHistorieScoreScreeningRonde5Jaar();
			event_leeftijd = afspraakEvent.getLeeftijd();
			event_leeftijd_per5 = afspraakEvent.getLeeftijdPer5();
			event_opkomst_vorige_afspraak = afspraakEvent.getOpkomstVorigeAfspraak();
			event_tehuis = afspraakEvent.getTehuis();
			event_voorgaande_afspraken = afspraakEvent.getVoorgaandeAfspraken();
			event_voorgaande_mammogrammen = afspraakEvent.getVoorgaandeMammogrammen();
			event_voorgaande_onderzoeken = afspraakEvent.getVoorgaandeOnderzoeken();
			event_voorgaande_screening_rondes = afspraakEvent.getVoorgaandeScreeningRondes();
			event_voorgaande_uitnodigingen = afspraakEvent.getVoorgaandeUitnodigingen();
			event_brief_type_uitnodiging = afspraakEvent.getBriefTypeUitnodiging();
			event_maand = afspraakEvent.getMaand();
			event_na_heraanmelding = afspraakEvent.getNaHeraanmelding();
			event_na_herinnering = afspraakEvent.getNaHerinnering();
			event_ronde_geforceerd = afspraakEvent.getRondeGeforceerd();
			event_uur = afspraakEvent.getUur();
			event_verzetten_reden = afspraakEvent.getVerzettenReden();
		}

		@Override
		public String toString()
		{
			try
			{
				return new ObjectMapper().setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY).writeValueAsString(this);
			}
			catch (JsonProcessingException e)
			{
				throw new RuntimeException(e);
			}
		}
	}

	@Override
	@Transactional(readOnly = true)
	public void resetPreferences()
	{
		minimaleLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name());
		useKansbereking = preferenceService.getBoolean(PreferenceKey.KANSBEREKENING_BK.toString());
		defaultOpkomstkansZonderKansberekening = preferenceService.getInteger(PreferenceKey.KANSBEREKENING_BK_TEST_DEFAULT_OPKOMSTKANS.name(), 50);
	}
}
