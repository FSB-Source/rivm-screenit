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
import java.util.Date;

import javax.persistence.FlushModeType;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.SmsStatus;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaKansberekeningAfspraakEvent;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOpkomstkans;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;
import nl.rivm.screenit.repository.mamma.MammaScreeningRondeRepository;
import nl.rivm.screenit.repository.mamma.MammaUploadBeeldenPogingRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.BerichtToSeRestBkService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseKwaliteitscontroleService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaVolgendeUitnodigingService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class MammaBaseFactoryImpl implements MammaBaseFactory
{
	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private UitnodigingsDao uitnodigingsDao;

	@Autowired
	private MammaBaseKansberekeningService kansberekeningService;

	@Autowired
	private MammaBaseStandplaatsService standplaatsService;

	@Autowired
	private MammaBaseKwaliteitscontroleService kwaliteitscontroleService;

	@Autowired
	private BerichtToSeRestBkService berichtToSeRestBkService;

	@Autowired
	private BerichtToBatchService berichtToBatchService;

	@Autowired
	private MammaVolgendeUitnodigingService volgendeUitnodigingService;

	@Autowired
	private MammaUploadBeeldenPogingRepository uploadBeeldenPogingRepository;

	@Autowired
	private MammaScreeningRondeRepository screeningRondeRepository;

	@Autowired
	@Qualifier("testModus")
	private Boolean testModus;

	@Override
	@Transactional
	public MammaScreeningRonde maakRonde(MammaDossier dossier, MammaStandplaatsRonde standplaatsRonde, boolean isGeforceerd)
	{
		var nu = dateSupplier.getDate();

		LOG.info("MammaScreeningRonde aanmaken voor client: {}", dossier.getClient().getId());

		var vorigeRonde = dossier.getLaatsteScreeningRonde();
		if (vorigeRonde != null && vorigeRonde.getStatus() == ScreeningRondeStatus.LOPEND)
		{
			vorigeRonde.setStatus(ScreeningRondeStatus.AFGEROND);
			vorigeRonde.setStatusDatum(dateSupplier.getDate());
			hibernateService.saveOrUpdate(vorigeRonde);

			logService.logGebeurtenis(LogGebeurtenis.MAMMA_RONDE_VERLOPEN, dossier.getClient(), Bevolkingsonderzoek.MAMMA);
		}

		var postcode = ApplicationContextProvider.getApplicationContext().getBean(ClientService.class).getGbaPostcode(dossier.getClient());

		var ronde = new MammaScreeningRonde();
		ronde.setStatus(ScreeningRondeStatus.LOPEND);
		ronde.setStatusDatum(nu);
		ronde.setCreatieDatum(nu);
		ronde.setAangemeld(true);
		ronde.setDossier(dossier);
		ronde.setStandplaatsRonde(standplaatsRonde);
		ronde.setUitnodigingsNr(getNextUniqueMammaUitnodigingsNr());
		ronde.setGeforceerd(isGeforceerd);
		ronde.setPostcode(postcode);
		ronde.setMinderValideOnderzoekZiekenhuis(false);

		dossier.getScreeningRondes().add(ronde);
		dossier.setLaatsteScreeningRonde(ronde);
		dossier.setStatus(DossierStatus.ACTIEF);

		hibernateService.saveOrUpdateAll(ronde, dossier);
		return ronde;
	}

	@Override
	@Transactional
	public MammaUitnodiging maakUitnodiging(MammaScreeningRonde screeningRonde, MammaStandplaatsRonde standplaatsRonde, BriefType briefType)
	{
		LOG.info("MammaUitnodiging aanmaken voor client: {} ", screeningRonde.getDossier().getClient().getId());

		var nu = dateSupplier.getDate();
		var uitnodiging = new MammaUitnodiging();
		uitnodiging.setCreatieDatum(nu);
		uitnodiging.setUitnodigingsDatum(nu);
		uitnodiging.setScreeningRonde(screeningRonde);
		uitnodiging.setStandplaatsRonde(standplaatsRonde);
		uitnodiging.setHerinnered(false);

		var afstand = standplaatsService.bepaalAfstand(standplaatsRonde.getStandplaats(), screeningRonde.getDossier().getClient());
		uitnodiging.setAfstand(afstand != null ? BigDecimal.valueOf(afstand) : null);

		screeningRonde.getUitnodigingen().add(uitnodiging);
		screeningRonde.setLaatsteUitnodiging(uitnodiging);

		hibernateService.saveOrUpdateAll(uitnodiging, screeningRonde);

		if (briefType != null)
		{
			var brief = briefService.maakBvoBrief(screeningRonde, briefType);
			uitnodiging.setBrief(brief);
			brief.setUitnodiging(uitnodiging);
			hibernateService.saveOrUpdateAll(brief, uitnodiging);
		}

		volgendeUitnodigingService.updateVolgendeUitnodigingBijNieuweUitnodiging(screeningRonde.getDossier());

		return uitnodiging;
	}

	@Override
	@Transactional
	public MammaAfspraak maakDummyAfspraak(MammaUitnodiging uitnodiging, Date vanaf, MammaCapaciteitBlok capaciteitBlok, MammaStandplaatsPeriode standplaatsPeriode,
		MammaVerzettenReden verzettenReden)
	{
		var afspraak = new MammaAfspraak();
		var persoon = uitnodiging.getScreeningRonde().getDossier().getClient().getPersoon();
		var postcode = persoon.getTijdelijkGbaAdres() != null ? persoon.getTijdelijkGbaAdres().getPostcode() : persoon.getGbaAdres().getPostcode();

		afspraak.setUitnodiging(uitnodiging);
		afspraak.setVanaf(vanaf);
		afspraak.setCapaciteitBlok(capaciteitBlok);
		afspraak.setStandplaatsPeriode(standplaatsPeriode);
		afspraak.setVerzettenReden(verzettenReden);
		afspraak.setStatus(MammaAfspraakStatus.GEPLAND);
		afspraak.setBezwaarAangevraagd(false);
		afspraak.setPostcode(postcode);
		afspraak.setCreatiedatum(dateSupplier.getDate());

		var afspraakEvent = new MammaKansberekeningAfspraakEvent();
		afspraakEvent.setAfspraak(afspraak);
		afspraak.setAfspraakEvent(afspraakEvent);

		var opkomstkans = new MammaOpkomstkans();
		opkomstkans.setAfspraak(afspraak);
		afspraak.setOpkomstkans(opkomstkans);

		kansberekeningService.resetPreferences();
		kansberekeningService.updateAfspraakEvent(afspraak, false);
		opkomstkans.setOpkomstkans(kansberekeningService.getOpkomstkans(afspraak));

		return afspraak;
	}

	@Transactional(propagation = Propagation.MANDATORY)
	@Override
	public MammaAfspraak maakAfspraak(MammaScreeningRonde screeningRonde, MammaCapaciteitBlok capaciteitBlok, Date vanaf, MammaStandplaatsPeriode standplaatsPeriode,
		MammaVerzettenReden verzettenReden, boolean notificeerBetrokkenSe, boolean stuurBerichtNaarSectra,
		boolean isGeforceerdeAfspraak, SmsStatus smsStatus)
	{
		hibernateService.getHibernateSession().setFlushMode(FlushModeType.COMMIT);
		var postcode = ApplicationContextProvider.getApplicationContext().getBean(ClientService.class).getGbaPostcode(screeningRonde.getDossier().getClient());

		var opkomstkans = new MammaOpkomstkans();

		var afspraak = new MammaAfspraak();
		afspraak.setCreatiedatum(dateSupplier.getDate());
		afspraak.setStandplaatsPeriode(standplaatsPeriode);
		afspraak.setVerzettenReden(verzettenReden);
		afspraak.setCapaciteitBlok(capaciteitBlok);
		afspraak.setVanaf(vanaf);
		afspraak.setStatus(MammaAfspraakStatus.GEPLAND);
		afspraak.setBezwaarAangevraagd(false);
		afspraak.setPostcode(postcode);
		afspraak.setOpkomstkans(opkomstkans);
		afspraak.setGeforceerdeAfspraak(isGeforceerdeAfspraak);
		afspraak.setSmsStatus(smsStatus);
		opkomstkans.setAfspraak(afspraak);

		var laatsteUitnodiging = screeningRonde.getLaatsteUitnodiging();
		laatsteUitnodiging.getAfspraken().add(afspraak);
		laatsteUitnodiging.setLaatsteAfspraak(afspraak);
		afspraak.setUitnodiging(laatsteUitnodiging);
		laatsteUitnodiging.setHerinnered(false);

		var afspraakEvent = new MammaKansberekeningAfspraakEvent();
		afspraakEvent.setAfspraak(afspraak);
		afspraak.setAfspraakEvent(afspraakEvent);

		kansberekeningService.updateAfspraakEvent(afspraak, false);
		opkomstkans.setOpkomstkans(kansberekeningService.getOpkomstkans(afspraak));

		hibernateService.saveOrUpdateAll(afspraakEvent, afspraak, opkomstkans, screeningRonde, laatsteUitnodiging);

		if (capaciteitBlok != null)
		{
			capaciteitBlok.getAfspraken().add(afspraak);
			hibernateService.saveOrUpdate(capaciteitBlok);
		}

		if (notificeerBetrokkenSe)
		{
			berichtToSeRestBkService.notificeerScreeningsEenhedenVerversenDaglijst(screeningRonde.getDossier().getClient());
		}

		if (stuurBerichtNaarSectra)
		{
			berichtToBatchService.queueMammaHL7v24BerichtUitgaand(screeningRonde.getDossier().getClient(), MammaHL7v24ORMBerichtStatus.SCHEDULED);
		}
		hibernateService.getHibernateSession().setFlushMode(FlushModeType.AUTO);

		return afspraak;
	}

	@Override
	public MammaUitstel maakUitstel(MammaScreeningRonde screeningRonde, MammaStandplaats standplaats, Date streefDatum, MammaUitstelReden uitstelReden)
	{
		var uitstel = new MammaUitstel();
		uitstel.setStandplaats(standplaats);
		uitstel.setStreefDatum(streefDatum);
		uitstel.setUitstelReden(uitstelReden);
		uitstel.setGemaaktOp(dateSupplier.getDate());
		uitstel.setScreeningRonde(screeningRonde);

		return uitstel;
	}

	@Override
	@Transactional
	public MammaMammografie maakMammografie(MammaOnderzoek onderzoek, InstellingGebruiker instellingGebruiker, MammaAnnotatieAfbeelding afbeelding)
	{
		var mammografie = new MammaMammografie();
		mammografie.setAfgerondOp(dateSupplier.getDate());
		mammografie.setIlmStatus(MammaMammografieIlmStatus.NIET_BESCHIKBAAR);
		mammografie.setIlmStatusDatum(dateSupplier.getDate());
		mammografie.setAfgerondDoor(instellingGebruiker);
		mammografie.setOnderzoek(onderzoek);
		mammografie.setVisueleInspectieAfbeelding(afbeelding);
		onderzoek.setMammografie(mammografie);

		hibernateService.saveOrUpdateAll(onderzoek, mammografie);

		kansberekeningService.dossierEventHerzien(onderzoek.getAfspraak().getUitnodiging().getScreeningRonde().getDossier());

		return mammografie;
	}

	@Override
	@Transactional
	public Long getNextUniqueMammaUitnodigingsNr()
	{
		long nextUitnodigingsNr = uitnodigingsDao.getNextUitnodigingsId();
		if (!Boolean.TRUE.equals(testModus) || !accessionNummerInGebruik(nextUitnodigingsNr))
		{
			return nextUitnodigingsNr;
		}
		else
		{
			return getNextUniqueMammaUitnodigingsNr();
		}
	}

	private boolean accessionNummerInGebruik(long uitnodigingsNummer)
	{
		return screeningRondeRepository.existsByUitnodigingsNr(uitnodigingsNummer) || uploadBeeldenPogingRepository.existsByAccessionNumber(
			uitnodigingsNummer);
	}

	@Override
	@Transactional
	public MammaAdhocMeekijkverzoek maakAdhocMeekijkverzoek(MammaOnderzoek onderzoek, String reden)
	{
		var meekijkverzoek = new MammaAdhocMeekijkverzoek();
		meekijkverzoek.setVolgnummer(kwaliteitscontroleService.getNextAdhocMeekrijkverzoekVolgnummer());
		meekijkverzoek.setOnderzoek(onderzoek);
		onderzoek.setMeekijkverzoek(meekijkverzoek);
		meekijkverzoek.setReden(reden);
		meekijkverzoek.setStatus(MammaVisitatieOnderzoekStatus.NIET_GEZIEN);
		hibernateService.saveOrUpdateAll(meekijkverzoek, onderzoek);
		return meekijkverzoek;
	}
}
