package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaBulkVerzettenFilter;
import nl.rivm.screenit.dto.mamma.afspraken.MammaKandidaatAfspraakDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningVerzetClientenDto;
import nl.rivm.screenit.main.dao.mamma.MammaAfspraakDao;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.transformer.AfspraakDatumResultTransformer;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.DigitaalBerichtTemplateType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDigitaalClientBericht;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BerichtToSeRestBkService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.service.mamma.MammaBaseKandidaatAfsprakenDeterminatiePeriode;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaDigitaalContactService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5SessionInThread;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static java.time.temporal.ChronoUnit.DAYS;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaAfspraakServiceImpl implements MammaAfspraakService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private MammaAfspraakDao afspraakDao;

	@Autowired
	private MammaBaseAfspraakService baseAfspraakService;

	@Autowired
	private LogService logService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private MammaBaseStandplaatsService baseStandplaatsService;

	@Autowired
	private BerichtToSeRestBkService berichtToSeRestBkService;

	@Autowired
	private MammaBaseKansberekeningService kansberekeningService;

	@Autowired
	private BaseBriefService baseBriefService;

	@Autowired
	private MammaDigitaalContactService digitaalContactService;

	@Autowired
	private DossierService dossierService;

	private static final int STREEF_INTERVAL = 2;

	private static final ExecutorService EXECUTOR_SERVICE = Executors.newSingleThreadExecutor();

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public Map<MammaScreeningsEenheid, List<Date>> getAfspraakDatums(MammaBlokkade blokkade)
	{
		List<AfspraakDatumResultTransformer.AfspraakDatum> afspraakDatums = afspraakDao.getAfspraakDatums(blokkade);
		Map<MammaScreeningsEenheid, List<Date>> screeningsEenheidAfspraakDatumsMap = new HashMap<>();

		afspraakDatums.forEach(afspraakDatum ->
		{
			if (screeningsEenheidAfspraakDatumsMap.containsKey(afspraakDatum.screeningsEenheid))
			{
				screeningsEenheidAfspraakDatumsMap.get(afspraakDatum.screeningsEenheid).add(afspraakDatum.datum);
			}
			else
			{
				List<Date> datums = new ArrayList<>();
				datums.add(afspraakDatum.datum);
				screeningsEenheidAfspraakDatumsMap.put(afspraakDatum.screeningsEenheid, datums);
			}
		});

		return screeningsEenheidAfspraakDatumsMap;
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public Date getDatumEersteGeplandeAfspraak(Long standplaatsPeriodeId)
	{
		return afspraakDao.getDatumEersteGeplandeAfspraak(standplaatsPeriodeId);
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public Date getDatumLaatsteGeplandeAfspraak(Long standplaatsPeriodeId)
	{
		return afspraakDao.getDatumLaatsteGeplandeAfspraak(standplaatsPeriodeId);
	}

	@Override
	public void bulkVerzetten(IMammaBulkVerzettenFilter filter, List<MammaAfspraak> afspraken, Account account, LocalDate verzettenVanDatum)
	{
		MammaStandplaatsPeriode standplaatsPeriode = filter.getStandplaatsPeriode();
		LocalDate vanaf = filter.getVanafLocalDate();
		LocalDate totEnMet = filter.getTotEnMetLocalDate();

		PlanningVerzetClientenDto verzetClientenDto = new PlanningVerzetClientenDto();
		verzetClientenDto.verzetStandplaatsPeriodeId = standplaatsPeriode.getId();
		Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen = preferenceService.getInteger(PreferenceKey.MAMMA_CAPACITEIT_VOLLEDIG_BENUT_TOT_EN_MET_AANTAL_WERKDAGEN.toString());
		kansberekeningService.resetPreferences();

		Set<LocalDate> afspraakDatums = new HashSet<>();
		afspraakDatums.add(verzettenVanDatum);

		for (MammaAfspraak bestaandeAfspraak : afspraken)
		{
			hibernateService.getHibernateSession().flush();

			var uitnodiging = bestaandeAfspraak.getUitnodiging();
			var dossier = uitnodiging.getScreeningRonde().getDossier();

			var voorlopigeOpkomstkans = kansberekeningService.getVoorlopigeOpkomstkans(uitnodiging, standplaatsPeriode, filter.getVerzettenReden());

			MammaBaseKandidaatAfsprakenDeterminatiePeriode baseKandidaatAfsprakenDeterminatiePeriode = SpringBeanProvider.getInstance()
				.getBean(MammaBaseKandidaatAfsprakenDeterminatiePeriode.class);

			var kandidaatAfspraak = baseKandidaatAfsprakenDeterminatiePeriode.getKandidaatAfspraakBulkVerzetten(dossier, standplaatsPeriode, vanaf, totEnMet,
				voorlopigeOpkomstkans, capaciteitVolledigBenutTotEnMetAantalWerkdagen);

			boolean annuleerVorigeAfspraak = bestaandeAfspraak.getVanaf().compareTo(dateSupplier.getDate()) > 0;

			var capaciteitBlok = hibernateService.load(MammaCapaciteitBlok.class, kandidaatAfspraak.getCapaciteitBlokDto().id);

			var digitaleBerichten = dossier.getLaatsteScreeningRonde().getBerichten();
			var emailAdres = dossier.getClient().getPersoon().getEmailadres();
			var kanEmailVersturen = !digitaleBerichten.isEmpty() && !bulkVerzettenAlleenViaBrief() && StringUtils.isNotBlank(emailAdres);

			var magMailVerzenden = false;

			if (kanEmailVersturen)
			{
				magMailVerzenden = bepaalOfErMailVerzondenMoetWorden(bestaandeAfspraak);
			}

			var nieuweAfspraak = baseAfspraakService.maakAfspraak(uitnodiging.getScreeningRonde(), capaciteitBlok,
				DateUtil.toUtilDate(kandidaatAfspraak.getVanaf().atDate(kandidaatAfspraak.getDatum())), filter.getStandplaatsPeriode(), filter.getVerzettenReden(),
				annuleerVorigeAfspraak, false, true, true, true, account, false);

			var nieuweAfspraakDate = DateUtil.toLocalDate(nieuweAfspraak.getVanaf());
			afspraakDatums.add(nieuweAfspraakDate);

			verzetClientenDto.clientIdSet.add(dossier.getClient().getId());

			verstuurBriefOfMail(nieuweAfspraak, magMailVerzenden);
		}

		baseConceptPlanningsApplicatie.verzetClienten(verzetClientenDto);

		berichtToSeRestBkService.notificeerSe(standplaatsPeriode.getScreeningsEenheid(), afspraakDatums);

		DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy");
		String melding = "Bulk verzetting: #" + afspraken.size() + " afspraken, reden: " + filter.getVerzettenReden() + ", vanaf: "
			+ vanaf.format(formatter) + ", tot/met: " + totEnMet.format(formatter);
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_AFSPRAAK_VERZET, account, melding, Bevolkingsonderzoek.MAMMA);
	}

	private boolean bepaalOfErMailVerzondenMoetWorden(MammaAfspraak bestaandeAfspraak)
	{
		var digitaleContacten = bestaandeAfspraak.getUitnodiging().getScreeningRonde().getBerichten();

		var laatsteDigitaleMailContact = digitaleContacten.stream().filter(d -> d.getDigitaalBerichtTemplateType() == DigitaalBerichtTemplateType.MAMMA_AFSPRAAK_BEVESTIGING)
			.max(Comparator.comparing(MammaDigitaalClientBericht::getCreatieMoment));

		var brieven = bestaandeAfspraak.getUitnodiging().getScreeningRonde().getBrieven();
		var laatsteAfspraakVerzetBrief = brieven.stream().filter(b -> b.getBriefType() == BriefType.MAMMA_AFSPRAAK_VERZET).max(Comparator.comparing(MammaBrief::getCreatieDatum));

		var creatieMomentDigitaalBericht = laatsteDigitaleMailContact.orElseGet(MammaDigitaalClientBericht::new).getCreatieMoment();

		if (creatieMomentDigitaalBericht == null)
		{
			return false;
		}

		if (laatsteAfspraakVerzetBrief.isPresent())
		{
			var briefCreatieMoment = laatsteAfspraakVerzetBrief.get().getCreatieDatum();

			if (creatieMomentDigitaalBericht.isBefore(DateUtil.toLocalDateTime(briefCreatieMoment)))
			{
				return false;
			}
		}
		var bestaandeAfspraakCreatieMoment = DateUtil.toLocalDateTime(bestaandeAfspraak.getCreatiedatum());
		return !creatieMomentDigitaalBericht.isBefore(bestaandeAfspraakCreatieMoment);
	}

	private void verstuurBriefOfMail(MammaAfspraak nieuweAfspraak, boolean mailVerzenden)
	{
		if (!mailVerzenden)
		{
			baseBriefService.maakBvoBrief(nieuweAfspraak.getUitnodiging().getScreeningRonde(), BriefType.MAMMA_AFSPRAAK_VERZET);
		}
		else
		{
			digitaalContactService.sendBevestigAfspraakMail(nieuweAfspraak);
		}
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public boolean kortVoorVolgendeRonde(MammaAfspraak afspraak)
	{
		MammaDossier dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		MammaStandplaats huidigeStandplaats = baseStandplaatsService.getStandplaatsMetPostcode(dossier.getClient());

		MammaScreeningRonde laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();

		if (laatsteScreeningRonde != null && huidigeStandplaats != null)
		{
			MammaStandplaatsRonde vorigeStandplaatsRondeClient = laatsteScreeningRonde.getStandplaatsRonde();
			LocalDate startMinimalePeriode;
			MammaStandplaatsPeriode eerstVolgendeStandplaatsPeriode = null;
			Date vandaag = dateSupplier.getDateMidnight();
			for (MammaStandplaatsRonde ronde : huidigeStandplaats.getStandplaatsRonden())
			{
				if (!ronde.equals(vorigeStandplaatsRondeClient))
				{
					for (MammaStandplaatsPeriode periode : ronde.getStandplaatsPerioden())
					{
						if (!periode.getTotEnMet().before(vandaag)
							&& (eerstVolgendeStandplaatsPeriode == null || eerstVolgendeStandplaatsPeriode.getVanaf().after(periode.getVanaf())))
						{
							eerstVolgendeStandplaatsPeriode = periode;
						}
					}
				}
			}
			if (eerstVolgendeStandplaatsPeriode != null)
			{
				if (huidigeStandplaats.equals(vorigeStandplaatsRondeClient.getStandplaats()))
				{
					int interval = eerstVolgendeStandplaatsPeriode.getStandplaatsRonde().getInterval() != null
						? eerstVolgendeStandplaatsPeriode.getStandplaatsRonde().getInterval().intValue()
						: STREEF_INTERVAL * 365;
					startMinimalePeriode = DateUtil.toLocalDate(laatsteScreeningRonde.getCreatieDatum())
						.plusDays(interval);
				}
				else
				{
					LocalDate vanaf = DateUtil.toLocalDate(eerstVolgendeStandplaatsPeriode.getVanaf());
					LocalDate totEnMet = DateUtil.toLocalDate(eerstVolgendeStandplaatsPeriode.getTotEnMet());
					startMinimalePeriode = vanaf.plusDays(DAYS.between(vanaf, totEnMet) / 2);
				}
				Integer minimaleIntervalVolgendeUitnodiging = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_UITNODIGINGEN.name());
				return startMinimalePeriode.minusDays(minimaleIntervalVolgendeUitnodiging).isBefore(DateUtil.toLocalDate(afspraak.getVanaf()));
			}
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public String controleerAfspraakInAndereLocatie(MammaKandidaatAfspraakDto kandidaatAfspraakDto, MammaDossier dossier)
	{
		MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, kandidaatAfspraakDto.getStandplaatsPeriodeId());
		MammaAfspraak laatsteAfspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(dossier.getLaatsteScreeningRonde());

		LocalDate nieuweAfspraakDatum = kandidaatAfspraakDto.getDatum();
		if (laatsteAfspraak == null)
		{
			TijdelijkAdres tijdelijkeLocatie = standplaatsPeriode.getStandplaatsRonde().getStandplaats().getTijdelijkeLocatie();

			if (tijdelijkeLocatie.getStartDatum() != null &&
				!nieuweAfspraakDatum.isBefore(DateUtil.toLocalDate(tijdelijkeLocatie.getStartDatum()))
				&& !nieuweAfspraakDatum.isAfter(DateUtil.toLocalDate(tijdelijkeLocatie.getEindDatum())))
			{
				return "andere.locatie.tijdelijk";
			}
		}
		else
		{
			MammaStandplaats vorigeStandplaats = laatsteAfspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats();
			MammaStandplaats nieuweStandplaats = standplaatsPeriode.getStandplaatsRonde().getStandplaats();
			if (vorigeStandplaats.equals(nieuweStandplaats))
			{

				MammaStandplaatsLocatie tijdelijkeLocatie = standplaatsPeriode.getStandplaatsRonde().getStandplaats().getTijdelijkeLocatie();
				if (tijdelijkeLocatie.getStartDatum() != null)
				{
					boolean vorigeAfspraakInTijdelijkeLocatie = false;
					boolean nieuweAfspraakInTijdelijkeLocatie = false;
					if (!DateUtil.compareBefore(laatsteAfspraak.getVanaf(), tijdelijkeLocatie.getStartDatum())
						&& !DateUtil.compareAfter(laatsteAfspraak.getVanaf(), tijdelijkeLocatie.getEindDatum()))
					{
						vorigeAfspraakInTijdelijkeLocatie = true;
					}

					if (!nieuweAfspraakDatum.isBefore(DateUtil.toLocalDate(tijdelijkeLocatie.getStartDatum()))
						&& !nieuweAfspraakDatum.isAfter(DateUtil.toLocalDate(tijdelijkeLocatie.getEindDatum())))
					{
						nieuweAfspraakInTijdelijkeLocatie = true;
					}

					if (vorigeAfspraakInTijdelijkeLocatie != nieuweAfspraakInTijdelijkeLocatie)
					{
						return "andere.locatie.actief.standplaats";
					}
				}
			}
		}
		return "";
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean magBevestigingsbriefAanmaken(MammaAfspraak afspraak)
	{
		List<MammaBrief> brieven = afspraak.getUitnodiging().getScreeningRonde().getBrieven();
		for (MammaBrief brief : brieven)
		{
			BriefType briefType = brief.getBriefType();
			if (!DateUtil.compareBefore(brief.getCreatieDatum(), afspraak.getCreatiedatum()) && BriefType.isMammaUitnodigingBrief(briefType))
			{
				return false;
			}
		}
		return true;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public void verzetAfsprakenNaarStandplaatsPlusBrievenKlaarzettenVoorAfdrukken(Map<Long, List<Long>> afsprakenTeVerplatsen, Account account)
	{
		afsprakenTeVerplatsen.entrySet().forEach(e -> EXECUTOR_SERVICE.submit(new AfsprakenVerplaatsenThread(e.getKey(), e.getValue(), account.getId())));
	}

	private boolean bulkVerzettenAlleenViaBrief()
	{
		return preferenceService.getBoolean(PreferenceKey.MAMMA_BULK_VERZETTEN_ALLEEN_BRIEF.name(), false);
	}

	private class AfsprakenVerplaatsenThread extends OpenHibernate5SessionInThread
	{

		private final Long standplaatsPeriodeId;

		private final List<Long> afsprakenIds;

		private final Long ingelogdeGebruikerId;

		AfsprakenVerplaatsenThread(Long standplaatsPeriodeId, List<Long> afsprakenIds, Long ingelogdeGebruikerId)
		{
			super(true);
			this.standplaatsPeriodeId = standplaatsPeriodeId;
			this.afsprakenIds = afsprakenIds;
			this.ingelogdeGebruikerId = ingelogdeGebruikerId;
		}

		@Override
		protected void runInternal()
		{
			List<MammaBrief> brieven = new ArrayList<>();
			MammaStandplaatsPeriode persistentStandplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class, standplaatsPeriodeId);
			InstellingGebruiker ingelogedeInstellingGebruiker = hibernateService.get(InstellingGebruiker.class, ingelogdeGebruikerId);
			PlanningVerzetClientenDto verzetClientenDto = new PlanningVerzetClientenDto();
			verzetClientenDto.verzetStandplaatsPeriodeId = persistentStandplaatsPeriode.getId();
			Set<LocalDate> afspraakDatums = new HashSet<>();
			for (Long afspraakId : afsprakenIds)
			{
				MammaAfspraak afspraak = hibernateService.get(MammaAfspraak.class, afspraakId);

				if (afspraak.equals(MammaScreeningRondeUtil.getLaatsteAfspraak(afspraak.getUitnodiging().getScreeningRonde())))
				{
					baseAfspraakService.maakAfspraak(afspraak.getUitnodiging().getScreeningRonde(), afspraak.getCapaciteitBlok(), afspraak.getVanaf(),
						persistentStandplaatsPeriode, MammaVerzettenReden.ONVOORZIENE_OMSTANDIGHEDEN, true, false, false, true, true, ingelogedeInstellingGebruiker, false);
					brieven.add(baseBriefService.maakBvoBrief(afspraak.getUitnodiging().getScreeningRonde(), BriefType.MAMMA_AFSPRAAK_VERZET));
					verzetClientenDto.clientIdSet.add(afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient().getId());
					afspraakDatums.add(DateUtil.toLocalDate(afspraak.getVanaf()));
				}
			}
			if (brieven.size() > 0)
			{
				baseConceptPlanningsApplicatie.verzetClienten(verzetClientenDto);
				berichtToSeRestBkService.notificeerSe(persistentStandplaatsPeriode.getScreeningsEenheid(), afspraakDatums);
				baseStandplaatsService.zetBrievenKlaarVoorStandplaatsVoorAfdrukken(brieven, persistentStandplaatsPeriode.getStandplaatsRonde().getStandplaats());
			}
		}

	}
}
