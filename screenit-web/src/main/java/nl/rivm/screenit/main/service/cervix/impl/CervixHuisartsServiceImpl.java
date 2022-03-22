package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.EnovationHuisartsDao;
import nl.rivm.screenit.dao.cervix.CervixHuisartsBaseDao;
import nl.rivm.screenit.huisartsenportaal.dto.ResetDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.main.service.OvereenkomstService;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsService;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsSyncService;
import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.cervix.CervixRegioMergedBrieven;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsAanmeldStatus;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsLocatieMutatieSoort;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierAanvraagStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.envers.ScreenitRevisionEntity;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenInstellingOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.OvereenkomstType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.GebruikersService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.WoonplaatsService;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.CodeGenerator;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.util.collections.CollectionUtils;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.envers.query.AuditEntity;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixHuisartsServiceImpl implements CervixHuisartsService
{

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private EnovationHuisartsDao huisartsDao;

	@Autowired
	private CervixHuisartsBaseDao cervixHuisartsBaseDao;

	@Autowired
	private CervixHuisartsSyncService cervixHuisartsSyncService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MailService mailService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private WoonplaatsService woonplaatsService;

	@Autowired
	private LogService logService;

	@Autowired
	private GebruikersService gebruikersService;

	@Autowired
	@Qualifier(value = "applicationUrl")
	private String applicationUrl;

	@Autowired
	private OvereenkomstService overeenkomstService;

	@Autowired
	@Qualifier(value = "huisartsPortaalUrl")
	private String huisartsPortaalUrl;

	@Override
	public CervixHuisarts getUitstrijkendArtsMetAgb(String agbCode)
	{
		var arts = cervixHuisartsBaseDao.getHuisarts(agbCode);
		if (arts == null)
		{
			arts = new CervixHuisarts();
			arts.setAanmeldStatus(CervixHuisartsAanmeldStatus.AANGEMAAKT);
			arts.setAgbcode(agbCode);
			var instellingGebruiker = new InstellingGebruiker();
			arts.setOrganisatieMedewerkers(new ArrayList<>());
			arts.getOrganisatieMedewerkers().add(instellingGebruiker);
			arts.setActief(true);
			var medewerker = new Gebruiker();
			medewerker.setActief(true);
			instellingGebruiker.setMedewerker(medewerker);
			instellingGebruiker.setOrganisatie(arts);
			instellingGebruiker.setActief(true);
			medewerker.setMedewerkercode(gebruikersService.getNextMedewerkercode());
			medewerker.setOrganisatieMedewerkers(new ArrayList<>());
			medewerker.getOrganisatieMedewerkers().add(instellingGebruiker);

			var postadresCervixHuisarts = new CervixHuisartsAdres();

			var enovationHuisarts = huisartsDao.getHuisartsByAgb(agbCode);
			if (enovationHuisarts != null)
			{
				var praktijknaam = enovationHuisarts.getPraktijknaam();
				var achternaam = enovationHuisarts.getAchternaam();
				if (StringUtils.isNotBlank(praktijknaam))
				{
					arts.setNaam(praktijknaam);
				}
				else if (StringUtils.isNotBlank(achternaam))
				{
					arts.setNaam(NaamUtil.getNaamHuisarts(enovationHuisarts));
				}

				if (StringUtils.isNotBlank(achternaam))
				{
					medewerker.setAchternaam(achternaam);
					medewerker.setVoorletters(enovationHuisarts.getVoorletters());
					medewerker.setTussenvoegsel(enovationHuisarts.getTussenvoegels());
				}

				var geslacht = enovationHuisarts.getGeslacht();
				if (geslacht != null)
				{
					switch (geslacht)
					{
					case MAN:
						medewerker.setAanhef(Aanhef.DHR);
						break;
					case VROUW:
						medewerker.setAanhef(Aanhef.MEVR);
						break;
					default:
						break;
					}
				}

				var locatieAdres = new CervixHuisartsAdres();
				var locatie = new CervixHuisartsLocatie();
				locatie.setZorgmailklantnummer(enovationHuisarts.getKlantnummer());
				locatie.setNaam("<empty>");
				locatie.setIban("<empty>");
				locatie.setIbanTenaamstelling("<empty>");
				locatie.setLocatieAdres(locatieAdres);
				locatie.setStatus(CervixLocatieStatus.KLANTNUMMER_NIET_GEVERIFIEERD);
				arts.getHuisartsLocaties().add(locatie);
				locatie.setHuisarts(arts);

				if (enovationHuisarts.getAdres() != null)
				{
					var postadresColonHuisarts = enovationHuisarts.getAdres();
					postadresCervixHuisarts.setWoonplaats(woonplaatsService.getWoonplaatsByName(postadresColonHuisarts.getPlaats()));
					postadresCervixHuisarts.setHuisnummer(postadresColonHuisarts.getHuisnummer());
					postadresCervixHuisarts.setHuisnummerToevoeging(postadresColonHuisarts.getHuisnummerToevoeging());
					postadresCervixHuisarts.setStraat(postadresColonHuisarts.getStraat());
					postadresCervixHuisarts.setPostcode(postadresColonHuisarts.getPostcode());
				}
			}
			arts.setPostadres(postadresCervixHuisarts);
			medewerker.setInlogMethode(InlogMethode.GEBRUIKERSNAAM_WACHTWOORD);
		}
		return arts;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public CervixHuisarts maakOfWijzigUitstrijkendArts(CervixHuisarts arts, InstellingGebruiker account) throws IllegalStateException
	{
		var artsGebruiker = arts.getOrganisatieMedewerkers().get(0).getMedewerker();

		if (artsGebruiker.getGebruikersnaam() == null)
		{
			artsGebruiker.setGebruikersnaam("ua-" + arts.getAgbcode());
		}
		if (CervixHuisartsAanmeldStatus.GEREGISTREERD.equals(arts.getAanmeldStatus()))
		{
			arts.setAanmeldStatus(CervixHuisartsAanmeldStatus.REGISTRATIE_KLAARGEZET);
		}
		arts.setMutatiedatum(currentDateSupplier.getDate());
		arts.setActief(Boolean.TRUE);
		arts.setNaam("Praktijk van " + NaamUtil.getTussenvoegselEnAchternaam(artsGebruiker));

		var postadres = arts.getPostadres();
		var gemeente = updateGemeente(postadres);

		if (gemeente == null)
		{
			throw new IllegalStateException("Er is geen gemeente gekoppeld aan deze woonplaats. Neem contact op met de helpdesk.");
		}

		if (gemeente.getScreeningOrganisatie() == null)
		{
			throw new IllegalStateException("Er is geen screeningsorganisatie gekoppeld aan de gemeente " + gemeente.getNaam() + ". Neem contact op met de helpdesk.");
		}

		for (var locatie : arts.getHuisartsLocaties())
		{
			var locatieAdres = locatie.getLocatieAdres();
			if (locatieAdres.getWoonplaats() == null)
			{
				locatieAdres.setWoonplaats(postadres.getWoonplaats());
			}
			if (locatieAdres.getHuisnummer() == null)
			{
				locatieAdres.setHuisnummer(postadres.getHuisnummer());
			}
			if (locatieAdres.getHuisnummerToevoeging() == null)
			{
				locatieAdres.setHuisnummerToevoeging(postadres.getHuisnummerToevoeging());
			}
			if (locatieAdres.getStraat() == null)
			{
				locatieAdres.setStraat(postadres.getStraat());
			}
			if (locatieAdres.getPostcode() == null)
			{
				locatieAdres.setPostcode(postadres.getPostcode());
			}
			hibernateService.saveOrUpdate(locatieAdres);
		}
		hibernateService.saveOrUpdateAll(artsGebruiker, arts, postadres);

		if (arts.getGemachtigde() == null)
		{
			var modelOvereenkomsten = overeenkomstService.getOvereenkomsten(OrganisatieType.HUISARTS, OvereenkomstType.ZAKELIJKE_OVEREENKOMST);
			Overeenkomst actueelsteModelOvereenkomst = null;
			for (var modelOvereenkomst : modelOvereenkomsten)
			{
				if (actueelsteModelOvereenkomst == null
					|| DateUtil.compareBefore(actueelsteModelOvereenkomst.getLaatsteUpdateDocument(), modelOvereenkomst.getLaatsteUpdateDocument()))
				{
					actueelsteModelOvereenkomst = modelOvereenkomst;
				}
			}
			if (actueelsteModelOvereenkomst != null)
			{
				var afgeslotenOvereenkomst = new AfgeslotenInstellingOvereenkomst();
				afgeslotenOvereenkomst.setInstelling(arts);
				afgeslotenOvereenkomst.setScreeningOrganisatie(postadres.getGbaGemeente().getScreeningOrganisatie());
				afgeslotenOvereenkomst.setStartDatum(currentDateSupplier.getDate());
				afgeslotenOvereenkomst.setOvereenkomst(actueelsteModelOvereenkomst);
				afgeslotenOvereenkomst.setTeAccoderen(true);
				overeenkomstService.saveOrUpdateOvereenkomst(afgeslotenOvereenkomst, null, account);
				arts.setGemachtigde(artsGebruiker);
			}
			hibernateService.saveOrUpdate(arts);
		}

		var gebruiker = arts.getOrganisatieMedewerkers().get(0).getMedewerker();

		var codeB = CodeGenerator.genereerCode(3, 3);
		gebruiker.setDatumWachtwoordAanvraag(currentDateSupplier.getDate());
		gebruiker.setWachtwoordChangeCode(codeB);

		var date = currentDateSupplier.getDate();
		var cervixRegioBrief = briefService.maakRegioBrief(gemeente.getScreeningOrganisatie(), BriefType.REGIO_REGISTRATIE_UITSTRIJKEND_HUISARTS, date, arts);
		cervixRegioBrief.setHuisarts(arts);
		hibernateService.saveOrUpdateAll(cervixRegioBrief);
		logService.logGebeurtenis(LogGebeurtenis.ORGANISATIE_NIEUW, account, "Huisarts: " + arts.getNaam(), Bevolkingsonderzoek.CERVIX);
		return arts;
	}

	@Override
	public void saveCervixHuisartsLocatie(List<CervixHuisartsLocatie> locaties)
	{
		if (!CollectionUtils.isEmpty(locaties))
		{
			for (var locatie : locaties)
			{
				hibernateService.saveOrUpdate(locatie.getLocatieAdres());
				hibernateService.saveOrUpdate(locatie);
				cervixHuisartsSyncService.sendData(locatie);
			}
		}
	}

	@Override
	public void sendRegistratieMail(CervixHuisarts huisarts)
	{
		try
		{
			var content = preferenceService.getString(PreferenceKey.HUISARTS_REG_EMAIL.name(), "U kunt zich registreren als huisarts met deze {link}.");
			var gebruiker = huisarts.getOrganisatieMedewerkers().get(0).getMedewerker();

			var achternaam = "";
			if (StringUtils.isNotBlank(gebruiker.getAchternaam()))
			{
				achternaam = " " + gebruiker.getAchternaam();
			}

			var tussenvoegsel = "";
			if (StringUtils.isNotBlank(gebruiker.getTussenvoegsel()))
			{
				tussenvoegsel = " " + gebruiker.getTussenvoegsel();
			}

			var voorletters = "";
			if (StringUtils.isNotBlank(gebruiker.getVoorletters()))
			{
				voorletters = " " + gebruiker.getVoorletters();
			}

			var aanhef = "";
			if (gebruiker.getAanhef() != null)
			{
				aanhef = " " + gebruiker.getAanhef().getNaam();
			}
			content = content.replaceAll("\\{aanhef\\}", aanhef);
			content = content.replaceAll("\\{achternaam\\}", achternaam);
			content = content.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
			content = content.replaceAll("\\{voorletters\\}", voorletters);

			var link = "<a href=\"" + huisartsPortaalUrl + "registreren/\">link</a>";
			content = content.replaceAll("\\{link\\}", link);

			mailService.queueMail(huisarts.getEmail(), preferenceService.getString(PreferenceKey.HUISARTS_REG_EMAILSUBJECT.name(), "Registreren huisarts"), content);
		}
		catch (Exception e)
		{
			LOG.error("Niet mogelijk om een registratiemail uit te sturen naar huisarts(" + huisarts.getId() + ") met agbcode " + huisarts.getAgbcode(), e);
		}
	}

	private Gemeente updateGemeente(CervixHuisartsAdres adres)
	{
		var gemeente = adres.getWoonplaats().getGemeente();
		adres.setGbaGemeente(gemeente);
		return gemeente;
	}

	@Override
	public void aanvraagLabformulieren(CervixLabformulierAanvraag labformulierAanvraag, CervixHuisartsLocatie huisartsLocatie, InstellingGebruiker instellingGebruiker)
	{
		var nu = currentDateSupplier.getDate();

		var so = huisartsLocatie.getHuisarts().getPostadres().getGbaGemeente().getScreeningOrganisatie();

		var labformulierenBrief = briefService.maakRegioBrief(so, BriefType.REGIO_UITSTRIJKEND_ARTS_LABFORMULIER, nu, null);

		var voorbladBrief = briefService.maakRegioBrief(so, BriefType.REGIO_UITSTRIJKEND_ARTS_VOORBLAD_LABFORMULIER, nu, null);

		labformulierAanvraag.setBrief(labformulierenBrief);
		labformulierAanvraag.setVoorbladBrief(voorbladBrief);
		labformulierAanvraag.setStatus(CervixLabformulierAanvraagStatus.AANGEVRAAGD);
		labformulierAanvraag.setAanvraagDatum(nu);
		labformulierAanvraag.setStatusDatum(nu);
		labformulierAanvraag.setInstellingGebruiker(instellingGebruiker);
		labformulierAanvraag.setHuisartsLocatie(huisartsLocatie);
		labformulierAanvraag.setMutatiedatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(labformulierAanvraag);

		cervixHuisartsSyncService.sendData(labformulierAanvraag);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateArts(CervixHuisarts arts, LogGebeurtenis logGebeurtenis, InstellingGebruiker ingelogdeGebruiker)
	{
		updateGemeente(arts.getPostadres());
		var instellingGebruiker = arts.getOrganisatieMedewerkers().get(0);
		arts.setMutatiedatum(currentDateSupplier.getDate());
		arts.setNaam(cervixHuisartsSyncService.getPraktijkNaam(instellingGebruiker.getMedewerker()));
		hibernateService.saveOrUpdateAll(instellingGebruiker.getMedewerker(), arts);
		cervixHuisartsSyncService.sendData(arts);
		logService.logGebeurtenis(logGebeurtenis, ingelogdeGebruiker, "Huisarts: " + arts.getNaam(), Bevolkingsonderzoek.CERVIX);
	}

	@Override
	public CervixRegioBrief getLaatsteRegistratieBrief(CervixHuisarts arts)
	{
		return cervixHuisartsBaseDao.getLaatsteRegistratieBrief(arts);
	}

	@Override
	public Date getMutatiedatumUitstrijkendArts(CervixHuisarts cervixHuisarts)
	{
		var query = EntityAuditUtil.createQuery(cervixHuisarts, hibernateService.getHibernateSession());
		query.addProjection(AuditEntity.revisionNumber().max());

		var lastRevision = (Number) query.getSingleResult();
		ScreenitRevisionEntity revisionEntity = null;
		if (lastRevision != null)
		{
			revisionEntity = hibernateService.get(ScreenitRevisionEntity.class, lastRevision);
			if (revisionEntity != null)
			{
				return new Date(revisionEntity.getTimestamp());
			}
		}
		return null;
	}

	@Override
	public List<CervixHuisarts> getUitstrijkendArtsen(int first, int count, String orderByProperty, boolean ascending, String agbCode, DateTime mutatiedatumVanaf,
		DateTime mutatiedatumTot, Gemeente... gemeentes)
	{
		return cervixHuisartsBaseDao.getUistrijkendArtsen(first, count, orderByProperty, ascending, agbCode, mutatiedatumVanaf, mutatiedatumTot, gemeentes);
	}

	@Override
	public long countUitstrijkendArts(String agbCode, DateTime mutatiedatumVanaf, DateTime mutatiedatumTot, Gemeente... gemeentes)
	{
		return cervixHuisartsBaseDao.countUitstrijkendArtsen(agbCode, mutatiedatumVanaf, mutatiedatumTot, gemeentes);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateLocatie(CervixHuisartsLocatie locatie)
	{
		updateGemeente(locatie.getLocatieAdres());
		locatie.setMutatiedatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdateAll(locatie, locatie.getLocatieAdres());
		cervixHuisartsSyncService.sendData(locatie);
	}

	@Override
	public boolean isAndereLocatieMetNaam(CervixHuisartsLocatie locatie)
	{

		return cervixHuisartsBaseDao.getCervixHuisartsLocatieWithName(locatie) != null;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void updateLabformulierAanvraag(CervixRegioMergedBrieven regioMergedBrieven)
	{
		var formulierAanvragen = cervixHuisartsBaseDao.getCervixLabformulierAanvraagFromMergedBrieven(regioMergedBrieven);
		if (!CollectionUtils.isEmpty(formulierAanvragen))
		{
			var isVolledigAfgedrukt = true;
			for (CervixLabformulierAanvraag aanvraag : formulierAanvragen)
			{
				var isVoorbladIsAfgedrukt = BriefUtil.isMergedBrievenGeprint(aanvraag.getVoorbladBrief());
				var isFormulierenAfgedrukt = BriefUtil.isMergedBrievenGeprint(aanvraag.getBrief());
				if (!isVoorbladIsAfgedrukt || !isFormulierenAfgedrukt)
				{
					isVolledigAfgedrukt = false;
					break;
				}
			}
			if (isVolledigAfgedrukt)
			{
				for (var aanvraag : formulierAanvragen)
				{
					aanvraag.setStatus(CervixLabformulierAanvraagStatus.AFGEDRUKT_EN_VERSTUURD);
					aanvraag.setStatusDatum(currentDateSupplier.getDate());
					hibernateService.saveOrUpdate(aanvraag);

					cervixHuisartsSyncService.sendData(aanvraag);
				}
			}
		}
	}

	@Override
	public void resetWachtwoord(CervixHuisarts huisarts, Account loggedInAccount)
	{
		var huisartsID = huisarts.getScreenitId();

		var resetDto = new ResetDto();
		resetDto.setHuisarts_id(huisartsID);

		cervixHuisartsSyncService.sendData(resetDto);
		logService.logGebeurtenis(LogGebeurtenis.WACHTWOORD_GERESET, loggedInAccount, "Huisarts: " + huisarts.getNaam());
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void inactiveerHuisarts(CervixHuisarts huisarts, InstellingGebruiker loggedInAccount)
	{
		var nu = currentDateSupplier.getDate();
		huisarts.setActief(false);
		saveOrUpdateArts(huisarts, LogGebeurtenis.ORGANISATIE_INACTIVEERD, loggedInAccount);
		huisarts.getHuisartsLocaties().forEach(locatie -> inactiveerLocatie(locatie, nu, loggedInAccount));
	}

	private void inactiveerLocatie(CervixHuisartsLocatie locatie, Date nu, InstellingGebruiker loggedInAccount)
	{
		locatie.setStatus(CervixLocatieStatus.INACTIEF);
		locatie.setMutatieSoort(CervixHuisartsLocatieMutatieSoort.GEINACTIVEERD);
		locatie.setMutatiedatum(currentDateSupplier.getDate());
		saveOrUpdateLocatie(locatie); 
		verwijderNogNietVerstuurdeLabformulierenVanLocatie(locatie, nu);
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_HUISARTSLOCATIE_VERWIJDERD, loggedInAccount,
			"Huisarts: " + locatie.getHuisarts().getNaam() + " locatie: " + locatie.getNaam(), Bevolkingsonderzoek.CERVIX);
	}

	private void verwijderNogNietVerstuurdeLabformulierenVanLocatie(CervixHuisartsLocatie locatie, Date nu)
	{
		List<CervixLabformulierAanvraag> aanvragen = cervixHuisartsBaseDao.getNogNietVerstuurdeCervixLabformulierAanvraagVanLocatie(locatie);
		aanvragen.forEach(aanvraag -> verwijderCervixLabformulierAanvraag(aanvraag, nu));
	}

	private void verwijderCervixLabformulierAanvraag(CervixLabformulierAanvraag aanvraag, Date nu)
	{
		aanvraag.setStatusDatum(nu);
		aanvraag.setStatus(CervixLabformulierAanvraagStatus.VERWIJDERD);
		hibernateService.saveOrUpdate(aanvraag);
		cervixHuisartsSyncService.sendData(aanvraag);
	}
}
