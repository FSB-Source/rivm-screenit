package nl.rivm.screenit.mamma.planning.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.math.BigDecimal;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.planning.index.PlanningClientIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.model.rapportage.PlanningStandplaatsPeriodeUitnodigenRapportageDto;
import nl.rivm.screenit.mamma.planning.model.rapportage.PlanningStandplaatsRondeUitnodigenRapportageDto;
import nl.rivm.screenit.mamma.planning.model.rapportage.PlanningUitnodigenRapportageDto;
import nl.rivm.screenit.mamma.planning.service.PlanningUitnodigenService;
import nl.rivm.screenit.mamma.planning.service.PlanningUitnodigingContext;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeRapportageStatus;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseKandidaatAfsprakenDeterminatiePeriode;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.rivm.screenit.service.mamma.impl.MammaKandidaatAfspraak;
import nl.rivm.screenit.service.mamma.impl.MammaOnvoldoendeVrijeCapaciteitException;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
@Slf4j
public class PlanningUitnodigenServiceImpl implements PlanningUitnodigenService
{

	private final Map<Long, Integer> screeningsEenheidLaatsteVolgNrMap = new HashMap<>();

	private final MammaBaseScreeningrondeService screeningrondeService;

	private final LogService logService;

	private final HibernateService hibernateService;

	private final MammaBaseFactory baseFactory;

	private final MammaBaseUitstelService baseUitstelService;

	private final MammaBaseKansberekeningService baseKansberekeningService;

	private final MammaBaseAfspraakService baseAfspraakService;

	private final InstellingService instellingService;

	public PlanningUitnodigenServiceImpl(MammaBaseScreeningrondeService screeningrondeService, LogService logService,
		HibernateService hibernateService, MammaBaseFactory baseFactory,
		MammaBaseUitstelService baseUitstelService, MammaBaseKansberekeningService baseKansberekeningService,
		MammaBaseAfspraakService baseAfspraakService, InstellingService instellingService)
	{
		this.screeningrondeService = screeningrondeService;
		this.logService = logService;
		this.hibernateService = hibernateService;
		this.baseFactory = baseFactory;
		this.baseUitstelService = baseUitstelService;
		this.baseKansberekeningService = baseKansberekeningService;
		this.baseAfspraakService = baseAfspraakService;
		this.instellingService = instellingService;
	}

	@Override
	public void uitnodigen(PlanningStandplaatsRonde standplaatsRonde, Set<PlanningClient> openUitnodigingClientSet, NavigableSet<PlanningClient> afspraakUitnodigingClientSet,
		PlanningUitnodigenRapportageDto rapportageDto, PlanningUitnodigingContext context)
	{
		MammaStandplaatsRonde mammaStandplaatsRonde = hibernateService.get(MammaStandplaatsRonde.class, standplaatsRonde.getId());

		MammaStandplaatsPeriode eersteMammaStandplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class,
			standplaatsRonde.getStandplaatsPeriodeNavigableSet().first().getId());

		baseKansberekeningService.resetPreferences();

		for (PlanningClient client : openUitnodigingClientSet)
		{
			BriefType briefType = screeningrondeService.bepaalBriefTypeVoorOpenUitnodiging(client.isSuspect(), client.getDoelgroep());

			maakRondeEnUitnodiging(getDossier(client), briefType, mammaStandplaatsRonde, false);
			voegToe(getStandplaatsPeriodeUitnodigenRapportage(rapportageDto, eersteMammaStandplaatsPeriode), briefType , client);
		}

		for (PlanningClient client : afspraakUitnodigingClientSet)
		{
			MammaDossier dossier = getDossier(client);
			BriefType briefType = BriefType.MAMMA_AFSPRAAK_UITNODIGING;
			BigDecimal voorlopigeOpkomstkans = baseKansberekeningService
				.getVoorlopigeOpkomstkans(dossier, eersteMammaStandplaatsPeriode, null, briefType);

			MammaKandidaatAfspraak kandidaatAfspraak;
			try
			{
				MammaBaseKandidaatAfsprakenDeterminatiePeriode baseKandidaatAfsprakenDeterminatiePeriode = SpringBeanProvider.getInstance()
					.getBean(MammaBaseKandidaatAfsprakenDeterminatiePeriode.class);
				kandidaatAfspraak = baseKandidaatAfsprakenDeterminatiePeriode.getKandidaatAfspraakUitnodiging(dossier, mammaStandplaatsRonde, voorlopigeOpkomstkans,
					context.capaciteitVolledigBenutTotEnMetAantalWerkdagen, context.afspraakBijUitnodigenVanafAantalWerkdagen);
			}
			catch (MammaOnvoldoendeVrijeCapaciteitException e)
			{
				ScreeningOrganisatie regio = mammaStandplaatsRonde.getStandplaats().getRegio();
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_ONVOLDOENDE_VRIJE_CAPACITEIT, List.of(regio),
					new LogEvent(mammaStandplaatsRonde.getStandplaats().getNaam()),
					Bevolkingsonderzoek.MAMMA);
				break;
			}

			MammaScreeningRonde mammaScreeningRonde = maakRondeEnUitnodiging(dossier, briefType, mammaStandplaatsRonde, false);
			MammaCapaciteitBlok capaciteitBlok = hibernateService.load(MammaCapaciteitBlok.class, kandidaatAfspraak.getCapaciteitBlokDto().id);
			MammaStandplaatsPeriode standplaatsPeriode = kandidaatAfspraak.getCapaciteitBlokDto().standplaatsPeriode;
			baseAfspraakService.maakAfspraak(mammaScreeningRonde, capaciteitBlok, DateUtil.toUtilDate(kandidaatAfspraak.getVanaf().atDate(kandidaatAfspraak.getDatum())),
				standplaatsPeriode, null, false, true, false, true, false, null, false);

			voegToe(getStandplaatsPeriodeUitnodigenRapportage(rapportageDto, standplaatsPeriode), briefType, client);
			if (context.uitnodigenOnderbreken)
			{
				LOG.info("Uitnodigen afgebroken.");
				break;
			}
		}

		standplaatsRondeToevoegen(standplaatsRonde);
	}

	@Override
	public void achtervangUitstel(PlanningStandplaatsRonde standplaatsRonde, Set<PlanningClient> uitTeStellenClientSet, PlanningUitnodigenRapportageDto rapportage)
	{
		MammaStandplaatsRonde mammaStandplaatsRonde = hibernateService.get(MammaStandplaatsRonde.class, standplaatsRonde.getId());

		uitstellen(mammaStandplaatsRonde, uitTeStellenClientSet, mammaStandplaatsRonde.getAchtervangStandplaats(), MammaUitstelReden.ACHTERVANG_UITSTEL);
		mammaStandplaatsRonde.setAchtervangToegepast(true);

		MammaStandplaatsPeriode laatsteMammaStandplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class,
			standplaatsRonde.getStandplaatsPeriodeNavigableSet().last().getId());
		getStandplaatsPeriodeUitnodigenRapportage(rapportage, laatsteMammaStandplaatsPeriode).setUitgesteldAchtervangUitstel((long) uitTeStellenClientSet.size());
	}

	@Override
	public void minderValideUitwijkUitstel(PlanningStandplaatsRonde standplaatsRonde, Set<PlanningClient> uitTeStellenClientSet, PlanningUitnodigenRapportageDto rapportage)
	{
		MammaStandplaatsRonde mammaStandplaatsRonde = hibernateService.get(MammaStandplaatsRonde.class, standplaatsRonde.getId());

		uitstellen(mammaStandplaatsRonde, uitTeStellenClientSet, mammaStandplaatsRonde.getMinderValideUitwijkStandplaats(),
			MammaUitstelReden.MINDER_VALIDE_UITWIJK_UITSTEL);

		MammaStandplaatsPeriode eersteMammaStandplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class,
			standplaatsRonde.getStandplaatsPeriodeNavigableSet().first().getId());
		getStandplaatsPeriodeUitnodigenRapportage(rapportage, eersteMammaStandplaatsPeriode).setUitgesteldMinderValideUitgewijktUitstel((long) uitTeStellenClientSet.size());
	}

	@Override
	public void clear()
	{
		screeningsEenheidLaatsteVolgNrMap.clear();
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public PlanningStandplaatsPeriodeUitnodigenRapportageDto getStandplaatsPeriodeUitnodigenRapportage(PlanningUitnodigenRapportageDto rapportageDto,
		MammaStandplaatsPeriode standplaatsPeriode)
	{
		synchronized (rapportageDto)
		{
			MammaStandplaatsRonde standplaatsRonde = standplaatsPeriode.getStandplaatsRonde();
			PlanningStandplaatsRondeUitnodigenRapportageDto standplaatsRondeUitnodigenRapportage = rapportageDto.getStandplaatsRondeUitnodigenRapportages().stream()
				.filter(element -> element.getStandplaatsRondeId().equals(standplaatsRonde.getId())).findAny().orElse(null);
			if (standplaatsRondeUitnodigenRapportage == null)
			{
				standplaatsRondeUitnodigenRapportage = new PlanningStandplaatsRondeUitnodigenRapportageDto();
				standplaatsRondeUitnodigenRapportage.setStandplaatsRondeId(standplaatsRonde.getId());

				standplaatsRondeUitnodigenRapportage.setStatus(MammaStandplaatsRondeRapportageStatus.FOUT);
				rapportageDto.getStandplaatsRondeUitnodigenRapportages().add(standplaatsRondeUitnodigenRapportage);
			}

			PlanningStandplaatsPeriodeUitnodigenRapportageDto standplaatsPeriodeUitnodigenRapportage = standplaatsRondeUitnodigenRapportage
				.getStandplaatsPeriodeUitnodigenRapportages()
				.stream()
				.filter(element -> element.getStandplaatsPeriodeId().equals(standplaatsPeriode.getId())).findAny().orElse(null);
			if (standplaatsPeriodeUitnodigenRapportage == null)
			{
				standplaatsPeriodeUitnodigenRapportage = new PlanningStandplaatsPeriodeUitnodigenRapportageDto();
				standplaatsPeriodeUitnodigenRapportage.setStandplaatsPeriodeId(standplaatsPeriode.getId());
				standplaatsPeriodeUitnodigenRapportage.setUitnodigenTotEnMet(standplaatsPeriode.getScreeningsEenheid().getUitnodigenTotEnMet());

				standplaatsRondeUitnodigenRapportage.getStandplaatsPeriodeUitnodigenRapportages().add(standplaatsPeriodeUitnodigenRapportage);
			}
			return standplaatsPeriodeUitnodigenRapportage;
		}
	}

	private void uitstellen(MammaStandplaatsRonde standplaatsRonde, Set<PlanningClient> uitTeStellenClientSet, MammaStandplaats uitstellenNaar, MammaUitstelReden uitstelReden)
	{
		for (PlanningClient client : uitTeStellenClientSet)
		{
			MammaScreeningRonde screeningRonde;
			if (client.getUitstelStandplaats() != null)
			{
				screeningRonde = getDossier(client).getLaatsteScreeningRonde();
			}
			else
			{
				screeningRonde = maakRondeEnUitnodiging(getDossier(client), null, standplaatsRonde, true);
			}

			MammaUitstel uitstel = baseFactory.maakUitstel(screeningRonde, uitstellenNaar, DateUtil.toUtilDate(client.getHuidigeStreefDatum()), uitstelReden);
			screeningRonde.setLaatsteUitstel(uitstel);
			screeningRonde.getUitstellen().add(uitstel);

			baseUitstelService.saveUitstel(uitstel, false, null);
			hibernateService.saveOrUpdateAll(uitstel, screeningRonde);
			client.setUitgenodigdHuidigeStandplaatsRonde(true); 
		}
	}

	private MammaDossier getDossier(PlanningClient client)
	{
		return hibernateService.get(Client.class, client.getId()).getMammaDossier();
	}

	private MammaScreeningRonde maakRondeEnUitnodiging(MammaDossier mammaDossier, BriefType uitnodigingsBriefType, MammaStandplaatsRonde mammaStandplaatsRonde, boolean uitstellen)
	{
		PlanningClient client = PlanningClientIndex.get(mammaDossier.getClient().getId());
		MammaScreeningRonde screeningRonde;
		if (client.getUitstelStandplaats() != null)
		{
			screeningRonde = mammaDossier.getLaatsteScreeningRonde();
			MammaUitstel uitstel = screeningRonde.getLaatsteUitstel();
			MammaUitnodiging uitnodiging = baseFactory.maakUitnodiging(screeningRonde, mammaStandplaatsRonde, uitnodigingsBriefType);
			uitstel.setUitnodiging(uitnodiging);
			hibernateService.saveOrUpdate(uitnodiging);
		}
		else
		{
			screeningRonde = baseFactory.maakRonde(mammaDossier, mammaStandplaatsRonde, false);
			if (!uitstellen)
			{
				baseFactory.maakUitnodiging(screeningRonde, mammaStandplaatsRonde, uitnodigingsBriefType);
			}
		}

		baseKansberekeningService.dossierEventHerzien(mammaDossier);

		return screeningRonde;
	}

	private void standplaatsRondeToevoegen(PlanningStandplaatsRonde voorgaandeStandplaatsRonde)
	{
		LOG.info("standplaatsRondeToevoegen voorgaandeStandplaatsRonde: " + voorgaandeStandplaatsRonde.getId());

		PlanningStandplaats standplaats = voorgaandeStandplaatsRonde.getStandplaats();
		PlanningScreeningsEenheid screeningsEenheid = voorgaandeStandplaatsRonde.getStandplaatsPeriodeNavigableSet().first().getScreeningsEenheid();

		if (standplaats.getStandplaatsRondeNavigableSet().last().equals(voorgaandeStandplaatsRonde))
		{

			MammaScreeningsEenheid mammaScreeningsEenheid = hibernateService.get(MammaScreeningsEenheid.class, screeningsEenheid.getId());
			MammaStandplaats mammaStandplaats = hibernateService.get(MammaStandplaats.class, standplaats.getId());
			MammaStandplaatsPeriode mammaStandplaatsPeriode = new MammaStandplaatsPeriode();
			MammaStandplaatsRonde mammaStandplaatsRonde = new MammaStandplaatsRonde();

			mammaScreeningsEenheid.getStandplaatsPerioden().add(mammaStandplaatsPeriode);
			mammaStandplaatsPeriode.setScreeningsEenheid(mammaScreeningsEenheid);
			mammaStandplaatsPeriode.setStandplaatsRonde(mammaStandplaatsRonde);
			mammaStandplaatsRonde.getStandplaatsPerioden().add(mammaStandplaatsPeriode);
			mammaStandplaatsRonde.setStandplaats(mammaStandplaats);
			mammaStandplaatsRonde.setAchtervangToegepast(false);
			mammaStandplaats.getStandplaatsRonden().add(mammaStandplaatsRonde);

			Date datum = DateUtil.toUtilDate(screeningsEenheid.getStandplaatsPeriodeNavigableSet().last().getTotEnMet().plusDays(1));
			mammaStandplaatsPeriode.setVanaf(datum);
			mammaStandplaatsPeriode.setTotEnMet(datum);
			mammaStandplaatsPeriode.setStandplaatsRondeVolgNr(1);

			Integer screeningsEenheidVolgNr = screeningsEenheidLaatsteVolgNrMap.containsKey(screeningsEenheid.getId())
				? screeningsEenheidLaatsteVolgNrMap.get(screeningsEenheid.getId()) + 1
				: screeningsEenheid.getStandplaatsPeriodeNavigableSet().last().getScreeningsEenheidVolgNr() + 1;

			mammaStandplaatsPeriode.setScreeningsEenheidVolgNr(screeningsEenheidVolgNr);

			screeningsEenheidLaatsteVolgNrMap.put(screeningsEenheid.getId(), screeningsEenheidVolgNr);

			hibernateService.saveOrUpdateAll(mammaStandplaatsRonde, mammaStandplaatsPeriode, mammaScreeningsEenheid, mammaStandplaats);
		}
	}

	private void voegToe(PlanningStandplaatsPeriodeUitnodigenRapportageDto standplaatsPeriodeUitnodigenRapportage, BriefType briefType, PlanningClient client)
	{
		client.setUitgenodigdHuidigeStandplaatsRonde(true);

		switch (briefType)
		{
		case MAMMA_OPEN_UITNODIGING:
			standplaatsPeriodeUitnodigenRapportage.setUitgenodigdOpen(standplaatsPeriodeUitnodigenRapportage.getUitgenodigdOpen() + 1);
			break;
		case MAMMA_AFSPRAAK_UITNODIGING:
			standplaatsPeriodeUitnodigenRapportage.setUitgenodigdAfspraak(standplaatsPeriodeUitnodigenRapportage.getUitgenodigdAfspraak() + 1);
			break;
		case MAMMA_UITNODIGING_MINDER_VALIDE:
			standplaatsPeriodeUitnodigenRapportage.setUitgenodigdMinderValide(standplaatsPeriodeUitnodigenRapportage.getUitgenodigdMinderValide() + 1);
			break;
		case MAMMA_UITNODIGING_SUSPECT:
			standplaatsPeriodeUitnodigenRapportage.setUitgenodigdSuspect(standplaatsPeriodeUitnodigenRapportage.getUitgenodigdSuspect() + 1);
			break;
		}

		if (client.getUitstelStandplaats() != null)
		{
			client.setUitgenodigdNaUitstel(true);
			standplaatsPeriodeUitnodigenRapportage.setUitgenodigdNaUitstel(standplaatsPeriodeUitnodigenRapportage.getUitgenodigdNaUitstel() + 1);
		}
	}
}
