
package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import nl.rivm.screenit.dao.colon.impl.RoosterDaoImpl;
import nl.rivm.screenit.main.service.OngeldigeBerichtenService;
import nl.rivm.screenit.main.service.colon.ColonDossierService;
import nl.rivm.screenit.main.service.colon.ColonVervolgonderzoekKeuzesDto;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.IGeografischeCoordinaten;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.berichten.cda.MeldingOngeldigCdaBericht;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonConclusie;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonGeenOnderzoekReden;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.project.ProjectInactiefReden;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.IFobtService;
import nl.rivm.screenit.service.impl.PersoonCoordinaten;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.IFOBTTestUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;

import org.apache.commons.lang.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.ObjectNotFoundException;
import org.joda.time.DateTime;
import org.joda.time.Interval;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
public class ColonDossierServiceImpl implements ColonDossierService
{
	private static final String NO_SHOW_COLOSCOPIE = "no show coloscopie";

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private OngeldigeBerichtenService ongeldigeBerichtenService;

	@Autowired
	private LogService logService;

	@Autowired
	private AfspraakService afspraakService;

	@Autowired
	private IFobtService ifobtService;

	@Autowired
	private ColonDossierBaseService dossierBaseService;

	@Autowired
	private CoordinatenService coordinatenService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void monsterNietBeoordeelbaar(IFOBTTest ifobtTest)
	{
		ifobtService.monsterNietBeoordeelbaar(ifobtTest);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void conclusieOpslaan(ColonIntakeAfspraak afspraak, ColonVervolgonderzoekKeuzesDto keuzes, InstellingGebruiker ingelogdeGebruiker, boolean wasOnHold)
	{
		ColonConclusie conclusie = afspraak.getConclusie();
		boolean nieuweConclusie = conclusie.getId() == null;
		String conclusieDiff = null;
		conclusie.setType(keuzes.conclusie);
		if (!keuzes.intakeConclusie && keuzes.conclusie == ColonConclusieType.DOORVERWIJZEN_NAAR_ANDER_CENTRUM && !keuzes.afspraakDirectMaken)
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
		ColonScreeningRonde screeningRonde = afspraak.getColonScreeningRonde();
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
			List<ColonBrief> aangemaakteConclusieBrieven = briefService.getNietGegenereerdeBrievenVanBriefTypes(screeningRonde.getBrieven(), BriefType.getColonConclusieBrieven());
			verwijderVervolgStappen(screeningRonde, aangemaakteConclusieBrieven);
		}
		if (!nieuweConclusie && StringUtils.isBlank(conclusieDiff))
		{
			return;
		}
		LocalDateTime nu = currentDateSupplier.getLocalDateTime();
		conclusie.setDatum(DateUtil.toUtilDate(nu));
		hibernateService.saveOrUpdate(conclusie);
		Client client = afspraak.getClient();
		ColonDossier dossier = screeningRonde.getDossier();
		switch (conclusie.getType())
		{
		case CT_COLOGRAFIE:
			dossier.setStatus(DossierStatus.INACTIEF);
			hibernateService.saveOrUpdate(dossier);
			screeningRonde.setStatusDatum(DateUtil.toUtilDate(nu.plusNanos(50000)));
			screeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
			hibernateService.saveOrUpdate(screeningRonde);
			break;
		case DOORVERWIJZEN_NAAR_ANDER_CENTRUM:
		{
			ColonIntakeAfspraak nieuweAfspraak = new ColonIntakeAfspraak();
			Kamer kamer = conclusie.getLocatieNieuweAfspraak();

			nieuweAfspraak.setLocation(kamer);
			nieuweAfspraak.setStartTime(conclusie.getDatumTijdNieuweAfspraak());
			nieuweAfspraak.setColonScreeningRonde(screeningRonde);
			nieuweAfspraak.setClient(client);
			nieuweAfspraak.setActief(true);
			nieuweAfspraak.setBezwaar(false);
			nieuweAfspraak.setDatumLaatsteWijziging(DateUtil.toUtilDate(nu));
			nieuweAfspraak.setAfspraaknummer(System.currentTimeMillis());
			nieuweAfspraak.setStatus(AfspraakStatus.GEPLAND);

			ColoscopieCentrum intakeLocatie = kamer.getColoscopieCentrum();
			AfspraakDefinitie afspraakDefinitie = intakeLocatie.getAfspraakDefinities().get(0);
			Integer duurAfspraakInMinuten = afspraakDefinitie.getDuurAfspraakInMinuten();
			nieuweAfspraak.setEndTime(new DateTime(nieuweAfspraak.getStartTime()).plusMinutes(duurAfspraakInMinuten).toDate());
			nieuweAfspraak.setDefinition(afspraakDefinitie);
			nieuweAfspraak.addDiscipline(afspraakDefinitie.getDisciplines().get(0));
			PersoonCoordinaten persoonCoordinaten = coordinatenService.getCoordinatenVanPersoon(client.getPersoon());
			IGeografischeCoordinaten intakeLocatieCoordinaten = intakeLocatie.getPostcodeCoordinaten();
			if (persoonCoordinaten.vanAdres != null && intakeLocatieCoordinaten != null)
			{
				double distance = BigDecimalUtil.berekenDistance(persoonCoordinaten.vanAdres, intakeLocatieCoordinaten);
				nieuweAfspraak.setAfstand(BigDecimal.valueOf(distance));
			}
			else
			{
				nieuweAfspraak.setAfstand(BigDecimal.valueOf(0));
			}
			client.getAfspraken().add(nieuweAfspraak);
			screeningRonde.getAfspraken().add(nieuweAfspraak);
			screeningRonde.setLaatsteAfspraak(nieuweAfspraak);
			afspraak.setNieuweAfspraak(nieuweAfspraak);
			nieuweAfspraak.setOudeAfspraak(afspraak);

			afspraakService.setAfspraakStatus(afspraak, AfspraakStatus.UITGEVOERD);

			hibernateService.saveOrUpdate(nieuweAfspraak);
			hibernateService.saveOrUpdate(screeningRonde);
			hibernateService.saveOrUpdate(client);
			RoosterItem roosterBlok = afspraakService.getVrijRoosterBlokVoorAfspraak(nieuweAfspraak);
			if (roosterBlok != null)
			{
				afspraak.setRoosterItem(roosterBlok);
				roosterBlok.getAfspraken().add(nieuweAfspraak);
				hibernateService.saveOrUpdateAll(nieuweAfspraak, roosterBlok);
			}
			break;
		}
		case NO_SHOW:
			briefService.maakColonBrief(screeningRonde, BriefType.COLON_INTAKE_NO_SHOW);
			break;
		case GEEN_VERVOLGONDERZOEK:
			BriefType briefType = null;
			switch (conclusie.getGeenOnderzoekReden())
			{
			case MEDISCHE_REDENEN:
				briefType = BriefType.COLON_BEVESTIGING_DEFINITIEVE_EXCLUSIE;
				break;
			default:
				briefType = BriefType.COLON_BEVESTIGING_TERUG_NAAR_SCREENING;
				break;
			}
			briefService.maakColonBrief(screeningRonde, briefType);
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
			BerichtZoekFilter filter = new BerichtZoekFilter();
			filter.setBsn(client.getPersoon().getBsn());
			filter.setMldBerichten(true);
			filter.setPaLabBerichten(true);
			List<MeldingOngeldigCdaBericht> berichten = ongeldigeBerichtenService.searchOngeldigeBerichten(filter, -1, -1, "datum", true);
			for (MeldingOngeldigCdaBericht bericht : berichten)
			{
				if (Boolean.TRUE.equals(bericht.getActief()) && Boolean.TRUE.equals(bericht.getHerstelbaar())
					&& Arrays.asList(RoosterDaoImpl.HERVERWERKING_MARKERS).stream().anyMatch(m -> bericht.getMelding().contains(m)))
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
		String diff = "";
		boolean diffs = false;
		diffs |= !Objects.equals(oldAfspraakConclusie.getAsaScore(), conclusie.getAsaScore());
		diffs |= ObjectUtils.compare(oldAfspraakConclusie.getDatumColoscopie(), conclusie.getDatumColoscopie()) != 0;
		diffs |= ObjectUtils.compare(oldAfspraakConclusie.getDatumTijdNieuweAfspraak(), conclusie.getDatumTijdNieuweAfspraak()) != 0;
		diffs |= !Objects.equals(oldAfspraakConclusie.getTekstCTColografieAnders(), conclusie.getTekstCTColografieAnders());
		diffs |= !Objects.equals(oldAfspraakConclusie.getGeenOnderzoekReden(), conclusie.getGeenOnderzoekReden());
		diffs |= !Objects.equals(oldAfspraakConclusie.getRedenCTColografie(), conclusie.getRedenCTColografie());
		diffs |= !Objects.equals(oldAfspraakConclusie.getType(), conclusie.getType());
		diffs |= !Objects.equals(oldAfspraakConclusie.getNoShowBericht(), conclusie.getNoShowBericht());
		diffs |= !Objects.equals(oldAfspraakConclusie.getNoShowColoscopie(), conclusie.getNoShowColoscopie());

		if (diffs)
		{
			diff = getOldConclusieInfo(oldAfspraakConclusie.getType(), oldAfspraakConclusie);
		}
		return diff;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderScannedAntwoordFormulier(ColonUitnodiging uitnodiging, InstellingGebruiker ingelogdeGebruiker)
	{
		uitnodiging = (ColonUitnodiging) HibernateHelper.deproxy(ModelProxyHelper.deproxy(uitnodiging));

		ColonScreeningRonde colonScreeningRonde = uitnodiging.getScreeningRonde();
		ColonDossier colonDossier = colonScreeningRonde.getDossier();
		Client client = colonDossier.getClient();

		DateTime datumTerugOntvangen = new DateTime(uitnodiging.getDatumTerugOntvangen());
		Interval range = new Interval(datumTerugOntvangen, datumTerugOntvangen.plusSeconds(3));

		ifobtService.verwijderScannedAntwoordFormulier(uitnodiging);

		List<ColonBrief> teVerwijderenBrieven = new ArrayList<>();
		List<ColonAfmelding> teVerwijderenAfmeldingen = new ArrayList<>();

		findAfmeldingenEnBrievenToDelete(teVerwijderenAfmeldingen, teVerwijderenBrieven, range, colonScreeningRonde.getAfmeldingen());

		findAfmeldingenEnBrievenToDelete(teVerwijderenAfmeldingen, teVerwijderenBrieven, range, colonDossier.getAfmeldingen());

		for (ColonAfmelding afmelding : teVerwijderenAfmeldingen)
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
		for (ColonAfmelding bestaandeAfmelding : colonScreeningRonde.getAfmeldingen())
		{
			if (nieuweLaatsteAfmelding == null || bestaandeAfmelding.getId() > nieuweLaatsteAfmelding.getId())
			{
				nieuweLaatsteAfmelding = bestaandeAfmelding;
			}
		}
		colonScreeningRonde.setLaatsteAfmelding(nieuweLaatsteAfmelding);

		nieuweLaatsteAfmelding = null;
		for (ColonAfmelding bestaandeAfmelding : colonDossier.getAfmeldingen())
		{
			if (nieuweLaatsteAfmelding == null || bestaandeAfmelding.getId() > nieuweLaatsteAfmelding.getId())
			{
				nieuweLaatsteAfmelding = bestaandeAfmelding;
			}
		}
		colonDossier.setLaatsteAfmelding(nieuweLaatsteAfmelding);

		verwijderVervolgStappen(colonScreeningRonde, teVerwijderenBrieven);

		for (ColonAfmelding afmelding : teVerwijderenAfmeldingen)
		{
			boolean magAfmeldingVerwijderen = false;
			if (colonScreeningRonde.equals(afmelding.getScreeningRonde()))
			{
				magAfmeldingVerwijderen = true;
			}
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
		ColonDossier colonDossier = colonScreeningRonde.getDossier();
		Client client = colonDossier.getClient();

		if (ScreeningRondeStatus.AFGEROND.equals(colonScreeningRonde.getStatus())
			&& (colonScreeningRonde.getLaatsteAfmelding() == null || colonScreeningRonde.getLaatsteAfmelding().getRondeHeropend())
			&& (colonDossier.getLaatsteAfmelding() == null || colonDossier.getLaatsteAfmelding().getHeraanmeldStatus() == AanvraagBriefStatus.VERWERKT))
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

		verwijderBrievenUitRonde(colonScreeningRonde, teVerwijderenBrieven);

		bepaalNieuweLaatsteBrief(colonScreeningRonde);

		hibernateService.saveOrUpdate(colonScreeningRonde);
		hibernateService.saveOrUpdate(colonDossier);
		hibernateService.saveOrUpdate(client);

		for (ColonBrief brief : teVerwijderenBrieven)
		{
			boolean magBriefVerwijderen = false;
			if (colonScreeningRonde.equals(brief.getScreeningRonde()))
			{
				magBriefVerwijderen = true;
			}
			if (client.equals(brief.getClient()))
			{
				magBriefVerwijderen = true;
			}

			if (magBriefVerwijderen)
			{
				verwijderBrief(brief);
			}
		}
	}

	private void bepaalNieuweLaatsteBrief(ColonScreeningRonde colonScreeningRonde)
	{
		ColonBrief nieuweLaatsteBrief = null;
		for (ColonBrief bestaandeBrief : colonScreeningRonde.getBrieven())
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
		for (ColonBrief brief : teVerwijderenBrieven)
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
		MergedBrieven mergedBrieven = brief.getMergedBrieven();
		brief.setMergedBrieven(null);
		if (mergedBrieven != null)
		{
			mergedBrieven.getBrieven().remove(brief);
			hibernateService.saveOrUpdate(mergedBrieven);
		}
		hibernateService.delete(brief);
	}

	private void findAfmeldingenEnBrievenToDelete(List<ColonAfmelding> teVerwijderenAfmeldingen, List<ColonBrief> teVerwijderenBrieven, Interval range,
		List<ColonAfmelding> afmeldingen)
	{
		for (ColonAfmelding afmelding : afmeldingen)
		{
			if (range.contains(new DateTime(afmelding.getAfmeldDatum())))
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
		ColonScreeningRonde screeningRonde = afspraak.getColonScreeningRonde();
		boolean heeftMdlVerslag = ColonScreeningRondeUtil.heeftAfgerondeVerslag(screeningRonde, VerslagType.MDL);
		return origConclusie != null
			&& !AfspraakStatus.isGeannuleerd(afspraak.getStatus())
			&& screeningRonde.getLaatsteAfspraak().equals(afspraak)
			&& !heeftMdlVerslag
			&& screeningRonde.getDossier().getLaatsteScreeningRonde().equals(screeningRonde);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void conclusieVerwijderen(ColonIntakeAfspraak afspraak, InstellingGebruiker loggedInInstellingGebruiker, ColonConclusieType origConclusie)
	{
		afspraak = (ColonIntakeAfspraak) HibernateHelper.deproxy(ModelProxyHelper.deproxy(afspraak));
		ColonScreeningRonde screeningRonde = afspraak.getColonScreeningRonde();
		ColonDossier dossier = screeningRonde.getDossier();
		Client client = dossier.getClient();
		ColonConclusie conclusie = afspraak.getConclusie();
		String melding = "Conclusie: " + origConclusie.getOmschrijving() + getOldConclusieInfo(origConclusie, conclusie);

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
		String melding = "";
		SimpleDateFormat formatD = new SimpleDateFormat("dd-MM-yyyy");
		SimpleDateFormat formatDT = new SimpleDateFormat("dd-MM-yyyy HH:mm");

		melding += "; ";
		if (conclusie.getDatum() != null)
		{
			melding += " datum/tijd " + formatDT.format(conclusie.getDatum()) + ", ";
		}
		InstellingGebruiker instellingGebruiker = conclusie.getInstellingGebruiker();
		if (instellingGebruiker != null)
		{
			Gebruiker medewerker = null;
			try
			{
				medewerker = instellingGebruiker.getMedewerker();
			}
			catch (ObjectNotFoundException e)
			{

				Long id = (Long) HibernateHelper.getId(instellingGebruiker);
				medewerker = hibernateService.load(InstellingGebruiker.class, id).getMedewerker();
			}
			melding += NaamUtil.getNaamGebruiker(medewerker) + ", ";
		}
		if (conclusie.getDatumColoscopie() != null && origConclusie.equals(ColonConclusieType.COLOSCOPIE))
		{
			melding += " datum coloscopie " + formatD.format(conclusie.getDatumColoscopie()) + ", ";
			if (Boolean.TRUE.equals(conclusie.getNoShowColoscopie()))
			{
				melding += " " + NO_SHOW_COLOSCOPIE + ", ";
			}
		}
		if (origConclusie.equals(ColonConclusieType.CT_COLOGRAFIE))
		{
			if (conclusie.getRedenCTColografie() != null)
			{
				melding += " CT-colografie reden " + conclusie.getRedenCTColografie().getOmschrijving() + ", ";
			}
			if (conclusie.getTekstCTColografieAnders() != null)
			{
				melding += " anders " + conclusie.getTekstCTColografieAnders() + ", ";
			}
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
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwijderIfobtUitslag(IFOBTTest buis, UploadDocument uploadDocument, InstellingGebruiker ingelogdeGebruiker)
	{
		ColonUitnodiging uitnodiging = (ColonUitnodiging) HibernateHelper.deproxy(ModelProxyHelper.deproxy(IFOBTTestUtil.getUitnodiging(buis)));

		ColonScreeningRonde colonScreeningRonde = uitnodiging.getScreeningRonde();
		ColonDossier colonDossier = colonScreeningRonde.getDossier();
		Client client = colonDossier.getClient();

		List<ColonBrief> teVerwijdernBrieven = new ArrayList<>();

		for (ColonBrief bestaandeBrief : colonScreeningRonde.getBrieven())
		{
			if (isBriefTeVerwijderen(buis, bestaandeBrief))
			{
				teVerwijdernBrieven.add(bestaandeBrief);
			}
		}

		List<ColonIntakeAfspraak> teVerwijderenAfspraken = new ArrayList<>();
		List<ColonHuisartsBericht> teVerwijderenHuisartsberichten = new ArrayList<>();

		if (IFOBTTestUtil.isOngunstig(buis) && !IFOBTTestUtil.heeftMeerdereOngunstigeUitslagenInZelfdeRonde(buis))
		{
			for (ColonIntakeAfspraak afspraak : colonScreeningRonde.getAfspraken())
			{
				teVerwijderenAfspraken.add(afspraak);
				client.getAfspraken().remove(afspraak);
				RoosterItem roosterItem = afspraak.getRoosterItem();
				if (roosterItem != null)
				{
					roosterItem.getAfspraken().remove(afspraak);
					hibernateService.saveOrUpdate(roosterItem);
				}
			}

			teVerwijderenHuisartsberichten.addAll(colonScreeningRonde.getHuisartsBerichten());
		}

		ifobtService.verwijderUitslag(buis, uploadDocument);
		clientService.projectClientInactiveren(client, ProjectInactiefReden.VALT_UIT_2DE_BUIS_PROJECT, Bevolkingsonderzoek.COLON);

		verwijderVervolgStappen(colonScreeningRonde, teVerwijdernBrieven);

		for (ColonIntakeAfspraak afspraak : teVerwijderenAfspraken)
		{
			colonScreeningRonde.getAfspraken().remove(afspraak);

			Afspraak nieuweAfspraak = afspraak.getNieuweAfspraak();
			if (nieuweAfspraak != null)
			{
				nieuweAfspraak.setOudeAfspraak(null);
				hibernateService.saveOrUpdate(nieuweAfspraak);
			}

			Afspraak oudeAfspraak = afspraak.getOudeAfspraak();
			if (oudeAfspraak != null)
			{
				oudeAfspraak.setNieuweAfspraak(null);
				hibernateService.saveOrUpdate(oudeAfspraak);
			}

			hibernateService.delete(afspraak);
		}
		colonScreeningRonde.setLaatsteAfspraak(null);
		for (ColonHuisartsBericht huisartsBericht : teVerwijderenHuisartsberichten)
		{
			client.getHuisartsBerichten().remove(huisartsBericht);
			colonScreeningRonde.getHuisartsBerichten().remove(huisartsBericht);
			hibernateService.delete(huisartsBericht);
		}
		hibernateService.saveOrUpdate(client);
		hibernateService.saveOrUpdate(colonScreeningRonde);

		logService.logGebeurtenis(LogGebeurtenis.IFOBT_UITSLAG_VERWIJDERD, ingelogdeGebruiker, client, "UitnodigingsId: " + uitnodiging.getUitnodigingsId(),
			Bevolkingsonderzoek.COLON);
	}

	private boolean isBriefTeVerwijderen(IFOBTTest buis, ColonBrief bestaandeBrief)
	{
		return IFOBTTestUtil.isEnigeUitgevoerdeIfobtInZelfdeRonde(buis) && BriefUtil.isUitslagBrief(bestaandeBrief)
			|| IFOBTTestUtil.isOngunstig(buis) && BriefUtil.isOngunstigeUitslagBrief(bestaandeBrief)
				&& !IFOBTTestUtil.heeftMeerdereOngunstigeUitslagenInZelfdeRonde(buis);
	}

	@Override
	public void vervangUitslagVerwijderenDocument(IFOBTTest buis, UploadDocument uploadDocument)
	{
		if (uploadDocument != null)
		{
			ColonUitnodiging uitnodiging = IFOBTTestUtil.getUitnodiging(buis);
			IFOBTTest gekoppeldeTest = uitnodiging.getGekoppeldeTest();
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
}
