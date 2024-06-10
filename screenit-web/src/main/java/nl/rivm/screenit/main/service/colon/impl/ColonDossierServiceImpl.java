package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.colon.impl.RoosterDaoImpl;
import nl.rivm.screenit.main.service.OngeldigeBerichtenService;
import nl.rivm.screenit.main.service.colon.ColonDossierService;
import nl.rivm.screenit.main.service.colon.ColonVervolgonderzoekKeuzesDto;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonGeenOnderzoekReden;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.DashboardService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.service.colon.ColonBaseFitService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.FITTestUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;

import org.apache.commons.lang.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.ObjectNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

import static nl.rivm.screenit.Constants.MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN;

@Service
@AllArgsConstructor
@Slf4j
public class ColonDossierServiceImpl implements ColonDossierService
{
	private final HibernateService hibernateService;

	private final ClientService clientService;

	private final BaseBriefService briefService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final OngeldigeBerichtenService ongeldigeBerichtenService;

	private final LogService logService;

	private final ColonBaseAfspraakService afspraakService;

	private final ColonBaseFitService fitService;

	private final ColonDossierBaseService dossierBaseService;

	private final OrganisatieParameterService organisatieParameterService;

	private final DashboardService dashboardService;

	@Override
	@Transactional
	public void monsterNietBeoordeelbaar(IFOBTTest ifobtTest)
	{
		fitService.monsterNietBeoordeelbaar(ifobtTest);
	}

	@Override
	@Transactional
	public void conclusieOpslaan(ColonIntakeAfspraak afspraak, ColonVervolgonderzoekKeuzesDto keuzes, InstellingGebruiker ingelogdeGebruiker,
		ColonConclusieType oudeConclusieType)
	{
		var conclusie = afspraak.getConclusie();
		var nieuweConclusie = conclusie.getId() == null;
		String conclusieDiff = null;
		conclusie.setType(keuzes.conclusie);
		if (!keuzes.intakeConclusie && keuzes.conclusie == ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM && !keuzes.verwijzing)
		{
			conclusie.setType(ColonConclusieType.CLIENT_WIL_ANDERE_INTAKELOKATIE);
		}
		else if (keuzes.intakeConclusie && conclusie.getType() == ColonConclusieType.GEEN_VERVOLGONDERZOEK)
		{
			if (!keuzes.redenGeenVervolgOnderzoek)
			{
				conclusie.setGeenOnderzoekReden(ColonGeenOnderzoekReden.VERZOEK_CLIENT);
			}
			else if (!keuzes.terugNaarScreening)
			{
				conclusie.setGeenOnderzoekReden(ColonGeenOnderzoekReden.MEDISCHE_REDENEN);
			}
			else
			{
				conclusie.setGeenOnderzoekReden(ColonGeenOnderzoekReden.getTerugNaarScreeningReden(keuzes.aantalJarenTerugNaarScreening));
			}

		}
		var screeningRonde = afspraak.getColonScreeningRonde();
		if (!nieuweConclusie)
		{
			ColonConclusie oldAfspraakConclusie = null;
			for (Object auditRow : EntityAuditUtil.getEntityHistory(afspraak, hibernateService.getHibernateSession(), false))
			{
				ColonIntakeAfspraak oldAfspraak = EntityAuditUtil.getRevisionEntity(auditRow);
				if (oldAfspraakConclusie == null)
				{
					oldAfspraakConclusie = oldAfspraak.getConclusie();
				}
				if (oldAfspraakConclusie != null)
				{
					for (Object auditConclusieRow : EntityAuditUtil.getEntityHistory(oldAfspraakConclusie, hibernateService.getHibernateSession(), false))
					{
						ColonConclusie oldAditAfspraakConclusie = EntityAuditUtil.getRevisionEntity(auditConclusieRow);
						conclusieDiff = diffConclusies(oldAditAfspraakConclusie, conclusie);
						break;
					}
					break;
				}
			}
			var aangemaakteConclusieBrieven = briefService.getNietGegenereerdeBrievenVanBriefTypes(screeningRonde.getBrieven(), BriefType.getColonConclusieBrieven());
			verwijderVervolgStappen(screeningRonde, aangemaakteConclusieBrieven);
		}
		if (!nieuweConclusie && StringUtils.isBlank(conclusieDiff) && !Boolean.TRUE.equals(keuzes.isDoorverwezenDoorInfolijn))
		{
			return;
		}

		if (Boolean.TRUE.equals(conclusie.getDoorverwijzingBevestigd()) && !ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM.equals(conclusie.getType()))
		{
			conclusie.setDoorverwijzingBevestigd(null);
		}

		var nu = currentDateSupplier.getLocalDateTime();
		conclusie.setDatum(DateUtil.toUtilDate(nu));
		hibernateService.saveOrUpdate(conclusie);
		var client = afspraak.getClient();
		var dossier = screeningRonde.getDossier();
		switch (conclusie.getType())
		{
		case CT_COLOGRAFIE:
			hibernateService.saveOrUpdate(dossier);
			screeningRonde.setStatusDatum(DateUtil.toUtilDate(nu.plusNanos(50000)));
			screeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
			hibernateService.saveOrUpdate(screeningRonde);
			break;
		case DOORVERWIJZEN_NAAR_ANDER_CENTRUM:
			if (!afspraakService.heeftOnafgerondeVerwijzingOmMedischeRedenen(afspraak))
			{
				conclusie.setDoorverwijzingBevestigd(true);
				hibernateService.saveOrUpdate(conclusie);
			}
			break;
		case CLIENT_WIL_ANDERE_INTAKELOKATIE:
			if (afspraak.getNieuweAfspraak() != null)
			{
				afspraakService.setAfspraakStatus(afspraak, AfspraakStatus.VERPLAATST);
			}
			break;
		case NO_SHOW:
			if (afspraak.getNieuweAfspraak() != null)
			{
				afspraakService.setAfspraakStatus(afspraak, AfspraakStatus.VERPLAATST);
			}
			else
			{
				briefService.maakBvoBrief(screeningRonde, BriefType.COLON_INTAKE_NO_SHOW);
			}
			break;
		case GEEN_VERVOLGONDERZOEK:
			BriefType briefType;
			if (ColonGeenOnderzoekReden.MEDISCHE_REDENEN.equals(conclusie.getGeenOnderzoekReden()))
			{
				briefType = BriefType.COLON_BEVESTIGING_DEFINITIEVE_EXCLUSIE;
			}
			else
			{
				briefType = BriefType.COLON_BEVESTIGING_TERUG_NAAR_SCREENING;
			}
			briefService.maakBvoBrief(screeningRonde, briefType);
			break;
		}
		if (!ColonConclusieType.CT_COLOGRAFIE.equals(conclusie.getType()))
		{
			if (dossier.getStatus().equals(DossierStatus.INACTIEF) && Boolean.TRUE.equals(dossier.getAangemeld()))
			{
				dossier.setStatus(DossierStatus.ACTIEF);
				hibernateService.saveOrUpdate(dossier);
			}
			if (screeningRonde.getStatus().equals(ScreeningRondeStatus.AFGEROND) && Boolean.TRUE.equals(screeningRonde.getAangemeld()))
			{
				screeningRonde.setStatusDatum(DateUtil.toUtilDate(nu.plusNanos(50000)));
				screeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
				hibernateService.saveOrUpdate(screeningRonde);
			}
		}
		if (AfspraakStatus.GEPLAND.equals(afspraak.getStatus()))
		{
			afspraak.setStatus(AfspraakStatus.UITGEVOERD);
			var filter = new BerichtZoekFilter();
			filter.setBsn(client.getPersoon().getBsn());
			filter.setMldBerichten(true);
			filter.setPaLabBerichten(true);
			var berichten = ongeldigeBerichtenService.searchOngeldigeBerichten(filter, -1, -1, "datum", true);
			for (var bericht : berichten)
			{
				if (Boolean.TRUE.equals(bericht.getActief()) && Boolean.TRUE.equals(bericht.getHerstelbaar())
					&& Arrays.stream(RoosterDaoImpl.HERVERWERKING_MARKERS).anyMatch(m -> bericht.getMelding().contains(m)))
				{
					ongeldigeBerichtenService.berichtOpnieuwAanbieden(bericht);
				}
			}
		}

		hibernateService.saveOrUpdate(afspraak);
		dossierBaseService.setVolgendeUitnodingVoorConclusie(afspraak);
		logService.logGebeurtenis(LogGebeurtenis.CONCLUSIE_VASTLEGGEN_WIJZIGEN, ingelogdeGebruiker, client, nieuweConclusie ? "Nieuw" : " Gewijzigd" + conclusieDiff,
			Bevolkingsonderzoek.COLON);
	}

	private String diffConclusies(ColonConclusie oldAfspraakConclusie, ColonConclusie conclusie)
	{
		var diff = "";
		boolean diffs;
		diffs = !Objects.equals(oldAfspraakConclusie.getAsaScore(), conclusie.getAsaScore());
		diffs |= ObjectUtils.compare(oldAfspraakConclusie.getDatumColoscopie(), conclusie.getDatumColoscopie()) != 0;
		diffs |= !Objects.equals(oldAfspraakConclusie.getGeenOnderzoekReden(), conclusie.getGeenOnderzoekReden());
		diffs |= !Objects.equals(oldAfspraakConclusie.getType(), conclusie.getType());
		diffs |= !Objects.equals(oldAfspraakConclusie.getNoShowBericht(), conclusie.getNoShowBericht());

		if (diffs)
		{
			diff = getOldConclusieInfo(oldAfspraakConclusie.getType(), oldAfspraakConclusie);
		}
		return diff;
	}

	@Override
	@Transactional
	public void verwijderScannedAntwoordFormulier(ColonUitnodiging uitnodiging, InstellingGebruiker ingelogdeGebruiker)
	{
		uitnodiging = (ColonUitnodiging) HibernateHelper.deproxy(ModelProxyHelper.deproxy(uitnodiging));

		var colonScreeningRonde = uitnodiging.getScreeningRonde();
		var colonDossier = colonScreeningRonde.getDossier();
		var client = colonDossier.getClient();

		var datumTerugOntvangen = uitnodiging.getDatumTerugOntvangen();
		var range = Range.closed(DateUtil.toLocalDateTime(datumTerugOntvangen), DateUtil.toLocalDateTime(datumTerugOntvangen).plusSeconds(3));

		fitService.verwijderScannedAntwoordFormulier(uitnodiging);

		var teVerwijderenBrieven = new ArrayList<ColonBrief>();
		var teVerwijderenAfmeldingen = new ArrayList<ColonAfmelding>();

		findAfmeldingenEnBrievenToDelete(teVerwijderenAfmeldingen, teVerwijderenBrieven, range, colonScreeningRonde.getAfmeldingen());

		findAfmeldingenEnBrievenToDelete(teVerwijderenAfmeldingen, teVerwijderenBrieven, range, colonDossier.getAfmeldingen());

		for (var afmelding : teVerwijderenAfmeldingen)
		{
			if (colonScreeningRonde.equals(afmelding.getScreeningRonde()))
			{
				colonScreeningRonde.getAfmeldingen().remove(afmelding);
			}
			if (colonDossier.equals(afmelding.getDossier()))
			{
				colonDossier.getAfmeldingen().remove(afmelding);
			}
		}

		ColonAfmelding nieuweLaatsteAfmelding = null;
		for (var bestaandeAfmelding : colonScreeningRonde.getAfmeldingen())
		{
			if (nieuweLaatsteAfmelding == null || bestaandeAfmelding.getId() > nieuweLaatsteAfmelding.getId())
			{
				nieuweLaatsteAfmelding = bestaandeAfmelding;
			}
		}
		colonScreeningRonde.setLaatsteAfmelding(nieuweLaatsteAfmelding);

		nieuweLaatsteAfmelding = null;
		for (var bestaandeAfmelding : colonDossier.getAfmeldingen())
		{
			if (nieuweLaatsteAfmelding == null || bestaandeAfmelding.getId() > nieuweLaatsteAfmelding.getId())
			{
				nieuweLaatsteAfmelding = bestaandeAfmelding;
			}
		}
		colonDossier.setLaatsteAfmelding(nieuweLaatsteAfmelding);

		verwijderVervolgStappen(colonScreeningRonde, teVerwijderenBrieven);

		for (var afmelding : teVerwijderenAfmeldingen)
		{
			var magAfmeldingVerwijderen = colonScreeningRonde.equals(afmelding.getScreeningRonde());
			if (colonDossier.equals(afmelding.getDossier()))
			{
				magAfmeldingVerwijderen = true;
			}
			if (magAfmeldingVerwijderen)
			{
				afmelding.setScreeningRonde(null);
				afmelding.setDossier(null);
				hibernateService.delete(afmelding);
			}
		}

		logService.logGebeurtenis(LogGebeurtenis.ANTWOORDFORMULIER_VERWIJDERD_GEZET, ingelogdeGebruiker, client, "UitnodigingsId: " + uitnodiging.getUitnodigingsId(),
			Bevolkingsonderzoek.COLON);
	}

	private void verwijderVervolgStappen(ColonScreeningRonde colonScreeningRonde, List<ColonBrief> teVerwijderenBrieven)
	{
		var colonDossier = colonScreeningRonde.getDossier();
		var client = colonDossier.getClient();

		if (ScreeningRondeStatus.AFGEROND.equals(colonScreeningRonde.getStatus())
			&& (colonScreeningRonde.getLaatsteAfmelding() == null || colonScreeningRonde.getLaatsteAfmelding().getRondeHeropend())
			&& (!AfmeldingUtil.isAfgerondeDefinitieveAfmelding(colonDossier.getLaatsteAfmelding())))
		{
			colonScreeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
			colonScreeningRonde.setAfgerondReden(null);
			colonScreeningRonde.setAangemeld(true);
		}

		if (Boolean.FALSE.equals(colonDossier.getAangemeld())
			&& (colonDossier.getLaatsteAfmelding() == null || colonDossier.getLaatsteAfmelding().getHeraanmeldStatus() == AanvraagBriefStatus.VERWERKT))
		{
			if (colonDossier.getInactiveerReden() == null)
			{

				colonDossier.setStatus(DossierStatus.ACTIEF);
				colonDossier.setInactiefVanaf(null);
			}
			colonDossier.setAangemeld(true);
		}

		verwijderBrieven(client, colonScreeningRonde, teVerwijderenBrieven);

		hibernateService.saveOrUpdate(colonScreeningRonde);
		hibernateService.saveOrUpdate(colonDossier);
		hibernateService.saveOrUpdate(client);
	}

	private void verwijderBrieven(Client client, ColonScreeningRonde screeningRonde, List<ColonBrief> teVerwijderenBrieven)
	{
		verwijderBrievenUitRonde(screeningRonde, teVerwijderenBrieven);
		bepaalNieuweLaatsteBrief(screeningRonde);

		for (var brief : teVerwijderenBrieven)
		{
			var magBriefVerwijderen = screeningRonde.equals(brief.getScreeningRonde()) || client.equals(brief.getClient());

			if (magBriefVerwijderen)
			{
				verwijderBrief(brief);
			}
		}
	}

	private void bepaalNieuweLaatsteBrief(ColonScreeningRonde colonScreeningRonde)
	{
		ColonBrief nieuweLaatsteBrief = null;
		for (var bestaandeBrief : colonScreeningRonde.getBrieven())
		{
			if (nieuweLaatsteBrief == null || DateUtil.compareAfter(bestaandeBrief.getCreatieDatum(), nieuweLaatsteBrief.getCreatieDatum()))
			{
				nieuweLaatsteBrief = bestaandeBrief;
			}
		}
		colonScreeningRonde.setLaatsteBrief(nieuweLaatsteBrief);
	}

	private void verwijderBrievenUitRonde(ColonScreeningRonde colonScreeningRonde, List<ColonBrief> teVerwijderenBrieven)
	{
		for (var brief : teVerwijderenBrieven)
		{
			if (colonScreeningRonde.equals(brief.getScreeningRonde()))
			{
				colonScreeningRonde.getBrieven().remove(brief);
			}
		}
	}

	private void verwijderBrief(ColonBrief brief)
	{
		brief.setClient(null);
		brief.setScreeningRonde(null);
		brief.setIntakeAfspraak(null);
		brief.setIfobtTest(null);
		var mergedBrieven = brief.getMergedBrieven();
		brief.setMergedBrieven(null);
		if (mergedBrieven != null)
		{
			mergedBrieven.getBrieven().remove(brief);
			hibernateService.saveOrUpdate(mergedBrieven);
		}
		hibernateService.delete(brief);
	}

	private void findAfmeldingenEnBrievenToDelete(List<ColonAfmelding> teVerwijderenAfmeldingen, List<ColonBrief> teVerwijderenBrieven, Range<LocalDateTime> range,
		List<ColonAfmelding> afmeldingen)
	{
		for (var afmelding : afmeldingen)
		{
			if (range.contains(DateUtil.toLocalDateTime(afmelding.getAfmeldDatum())))
			{
				if (afmelding.getAfmeldingAanvraag() != null)
				{
					teVerwijderenBrieven.add(afmelding.getAfmeldingAanvraag());
				}
				if (afmelding.getAfmeldingBevestiging() != null)
				{
					teVerwijderenBrieven.add(afmelding.getAfmeldingBevestiging());
				}
				if (afmelding.getHandtekeningDocumentAfmelding() != null)
				{
					hibernateService.delete(afmelding.getHandtekeningDocumentAfmelding());
				}
				if (afmelding.getHeraanmeldAanvraag() != null)
				{
					teVerwijderenBrieven.add(afmelding.getHeraanmeldAanvraag());
				}
				if (afmelding.getHeraanmeldBevestiging() != null)
				{
					teVerwijderenBrieven.add(afmelding.getHeraanmeldBevestiging());
				}
				if (afmelding.getHandtekeningDocumentHeraanmelding() != null)
				{
					hibernateService.delete(afmelding.getHandtekeningDocumentHeraanmelding());
				}
				teVerwijderenAfmeldingen.add(afmelding);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean magConclusieAanpassenVerwijderen(ColonIntakeAfspraak afspraak, ColonConclusieType origConclusie)
	{
		var screeningRonde = afspraak.getColonScreeningRonde();
		var heeftMdlVerslag = ColonScreeningRondeUtil.heeftAfgerondeVerslag(screeningRonde, VerslagType.MDL);
		return origConclusie != null
			&& !AfspraakStatus.isGeannuleerd(afspraak.getStatus())
			&& screeningRonde.getLaatsteAfspraak().equals(afspraak)
			&& !heeftMdlVerslag
			&& screeningRonde.getDossier().getLaatsteScreeningRonde().equals(screeningRonde);
	}

	@Override
	@Transactional
	public void conclusieVerwijderen(ColonIntakeAfspraak afspraak, InstellingGebruiker loggedInInstellingGebruiker, ColonConclusieType origConclusie)
	{
		afspraak = (ColonIntakeAfspraak) HibernateHelper.deproxy(ModelProxyHelper.deproxy(afspraak));
		var screeningRonde = afspraak.getColonScreeningRonde();
		var dossier = screeningRonde.getDossier();
		var client = dossier.getClient();
		var conclusie = afspraak.getConclusie();
		var melding = "Conclusie: " + origConclusie.getOmschrijving() + getOldConclusieInfo(origConclusie, conclusie);

		var nietGegenereerdeConclusieBrieven = briefService.getNietGegenereerdeBrievenVanBriefTypes(screeningRonde.getBrieven(), BriefType.getColonConclusieBrieven());

		verwijderBrieven(client, screeningRonde, nietGegenereerdeConclusieBrieven);

		afspraak.setStatus(AfspraakStatus.GEPLAND);
		afspraak.setConclusie(null);
		if (dossier.getStatus().equals(DossierStatus.INACTIEF) && Boolean.TRUE.equals(dossier.getAangemeld()))
		{
			dossier.setStatus(DossierStatus.ACTIEF);
			hibernateService.saveOrUpdate(dossier);
		}
		if (screeningRonde.getStatus().equals(ScreeningRondeStatus.AFGEROND) && Boolean.TRUE.equals(screeningRonde.getAangemeld()))
		{
			screeningRonde.setStatusDatum(DateUtil.toUtilDate(currentDateSupplier.getLocalDateTime().plusNanos(50000)));
			screeningRonde.setStatus(ScreeningRondeStatus.LOPEND);
			hibernateService.saveOrUpdate(screeningRonde);
		}
		hibernateService.delete(conclusie);
		hibernateService.saveOrUpdate(afspraak);
		dossierBaseService.setDatumVolgendeUitnodiging(dossier, ColonUitnodigingsintervalType.GEPLANDE_INTAKE_AFSPRAAK);

		logService.logGebeurtenis(LogGebeurtenis.CONCLUSIE_VERWIJDEREN, loggedInInstellingGebruiker, client, melding, Bevolkingsonderzoek.COLON);
		hibernateService.getHibernateSession().flush();
	}

	private String getOldConclusieInfo(ColonConclusieType origConclusie, ColonConclusie conclusie)
	{
		var melding = "";
		var formatD = new SimpleDateFormat("dd-MM-yyyy");
		var formatDT = new SimpleDateFormat("dd-MM-yyyy HH:mm");

		melding += "; ";
		if (conclusie.getDatum() != null)
		{
			melding += " datum/tijd " + formatDT.format(conclusie.getDatum()) + ", ";
		}
		var instellingGebruiker = conclusie.getInstellingGebruiker();
		if (instellingGebruiker != null)
		{
			Gebruiker medewerker;
			try
			{
				medewerker = instellingGebruiker.getMedewerker();
			}
			catch (ObjectNotFoundException e)
			{

				var id = (Long) HibernateHelper.getId(instellingGebruiker);
				medewerker = hibernateService.load(InstellingGebruiker.class, id).getMedewerker();
			}
			melding += NaamUtil.getNaamGebruiker(medewerker) + ", ";
		}
		if (conclusie.getDatumColoscopie() != null && origConclusie.equals(ColonConclusieType.COLOSCOPIE))
		{
			melding += " datum coloscopie " + formatD.format(conclusie.getDatumColoscopie()) + ", ";
		}
		if (conclusie.getAsaScore() != null)
		{
			melding += " asaScore " + conclusie.getAsaScore() + ", ";
		}

		if (melding.endsWith(", "))
		{
			melding = melding.substring(0, melding.length() - 2);
		}
		return melding;
	}

	@Override
	@Transactional
	public void verwijderIfobtUitslag(IFOBTTest buis, UploadDocument uploadDocument, InstellingGebruiker ingelogdeGebruiker)
	{
		var uitnodiging = (ColonUitnodiging) HibernateHelper.deproxy(ModelProxyHelper.deproxy(FITTestUtil.getUitnodiging(buis)));

		var screeningRonde = buis.getColonScreeningRonde();
		var client = screeningRonde.getDossier().getClient();

		var teVerwijderenBrieven = new ArrayList<ColonBrief>();

		for (var bestaandeBrief : screeningRonde.getBrieven())
		{
			if (isBriefTeVerwijderen(buis, bestaandeBrief))
			{
				teVerwijderenBrieven.add(bestaandeBrief);
			}
		}

		var teVerwijderenAfspraken = new ArrayList<ColonIntakeAfspraak>();
		var teVerwijderenHuisartsberichten = new ArrayList<ColonHuisartsBericht>();

		if (FITTestUtil.isOngunstig(buis) && !FITTestUtil.heeftMeerdereOngunstigeUitslagenInZelfdeRonde(buis))
		{
			for (var afspraak : screeningRonde.getAfspraken())
			{
				teVerwijderenAfspraken.add(afspraak);
				client.getAfspraken().remove(afspraak);
				var roosterItem = afspraak.getRoosterItem();
				if (roosterItem != null)
				{
					roosterItem.getAfspraken().remove(afspraak);
					hibernateService.saveOrUpdate(roosterItem);
				}
			}

			teVerwijderenHuisartsberichten.addAll(screeningRonde.getHuisartsBerichten());
		}

		fitService.verwijderUitslag(buis, uploadDocument);
		clientService.projectClientInactiveren(client, ProjectInactiefReden.VALT_UIT_2DE_BUIS_PROJECT, Bevolkingsonderzoek.COLON);

		verwijderVervolgStappen(screeningRonde, teVerwijderenBrieven);

		for (var afspraak : teVerwijderenAfspraken)
		{
			screeningRonde.getAfspraken().remove(afspraak);

			var nieuweAfspraak = afspraak.getNieuweAfspraak();
			if (nieuweAfspraak != null)
			{
				nieuweAfspraak.setOudeAfspraak(null);
				hibernateService.saveOrUpdate(nieuweAfspraak);
			}

			var oudeAfspraak = afspraak.getOudeAfspraak();
			if (oudeAfspraak != null)
			{
				oudeAfspraak.setNieuweAfspraak(null);
				hibernateService.saveOrUpdate(oudeAfspraak);
			}

			hibernateService.delete(afspraak);
		}
		screeningRonde.setLaatsteAfspraak(null);
		for (var huisartsBericht : teVerwijderenHuisartsberichten)
		{
			client.getHuisartsBerichten().remove(huisartsBericht);
			screeningRonde.getHuisartsBerichten().remove(huisartsBericht);
			hibernateService.delete(huisartsBericht);
		}
		hibernateService.saveOrUpdate(client);
		hibernateService.saveOrUpdate(screeningRonde);

		logService.logGebeurtenis(LogGebeurtenis.IFOBT_UITSLAG_VERWIJDERD, ingelogdeGebruiker, client, "UitnodigingsId: " + uitnodiging.getUitnodigingsId(),
			Bevolkingsonderzoek.COLON);
	}

	private boolean isBriefTeVerwijderen(IFOBTTest buis, ColonBrief bestaandeBrief)
	{
		return FITTestUtil.isEnigeUitgevoerdeFITInZelfdeRonde(buis) && BriefUtil.isUitslagBrief(bestaandeBrief)
			|| FITTestUtil.isOngunstig(buis) && BriefUtil.isOngunstigeUitslagBrief(bestaandeBrief)
			&& !FITTestUtil.heeftMeerdereOngunstigeUitslagenInZelfdeRonde(buis);
	}

	@Override
	@Transactional
	public void vervangUitslagVerwijderenDocument(IFOBTTest buis, UploadDocument uploadDocument)
	{
		if (uploadDocument != null)
		{
			var uitnodiging = FITTestUtil.getUitnodiging(buis);
			var gekoppeldeTest = uitnodiging.getGekoppeldeTest();
			if (gekoppeldeTest != null)
			{
				gekoppeldeTest.setVerwijderbrief(uploadDocument);
				hibernateService.saveOrUpdateAll(uploadDocument, gekoppeldeTest);
			}
		}
		else
		{
			throw new IllegalStateException("Geen upload document");
		}
	}

	@Override
	@Transactional
	public boolean setUitslagenGecontroleerdEnUpdateDashboard(LogRegel logRegel, InstellingGebruiker medewerker, DashboardStatus dashboardStatus)
	{
		var dossier = logRegel.getClient().getColonDossier();
		var signaleringsTermijn = organisatieParameterService.getOrganisatieParameter(null, OrganisatieParameterKey.COLON_SIGNALERINGSTERMIJN_MISSENDE_UITSLAGEN, 30);

		var nu = currentDateSupplier.getLocalDate();
		var laatstGesignaleerdeIfobt = fitService.getLaatsteFitMetMissendeUitslagVanDossier(dossier,
			nu.minusDays(MAX_AANTAL_DAGEN_TERUGKIJKEN_CONTROLE_MISSENDE_UITSLAGEN), nu.minusDays(signaleringsTermijn)).orElse(null);

		var isGedowngrade = dashboardService.updateLogRegelMetDashboardStatus(logRegel, medewerker.getMedewerker().getGebruikersnaam(), dashboardStatus);

		logService.logGebeurtenis(LogGebeurtenis.COLON_CONTROLE_MISSENDE_UITSLAGEN_MATCH_GECONTROLEERD, medewerker, dossier.getClient(), Bevolkingsonderzoek.COLON);

		if (laatstGesignaleerdeIfobt == null)
		{
			LOG.warn("Er zijn geen gesignaleerde IFOBTs gevonden voor dossier {}", dossier.getId());
		}
		else
		{
			dossier.setDatumLaatstGecontroleerdeSignalering(laatstGesignaleerdeIfobt.getAnalyseDatum());
			hibernateService.saveOrUpdate(dossier);
		}
		return isGedowngrade;
	}
}
