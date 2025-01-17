package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.JoinType;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.huisartsenportaal.dto.ResetDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.main.service.OvereenkomstService;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsService;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsSyncService;
import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruiker_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres_;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie_;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag;
import nl.rivm.screenit.model.cervix.CervixLabformulierAanvraag_;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.cervix.CervixRegioMergedBrieven;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsAanmeldStatus;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsLocatieMutatieSoort;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierAanvraagStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenInstellingOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.OvereenkomstType;
import nl.rivm.screenit.repository.cervix.CervixHuisartsLocatieRepository;
import nl.rivm.screenit.repository.cervix.CervixHuisartsRepository;
import nl.rivm.screenit.repository.cervix.CervixLabformulierAanvraagRepository;
import nl.rivm.screenit.repository.cervix.CervixRegioBriefRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.EnovationHuisartsService;
import nl.rivm.screenit.service.GebruikersService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.WoonplaatsService;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.specification.algemeen.BriefSpecification;
import nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification;
import nl.rivm.screenit.specification.cervix.CervixLabformulierAanvraagSpecification;
import nl.rivm.screenit.specification.cervix.CervixRegioBriefSpecification;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.CodeGenerator;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.util.collections.CollectionUtils;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification.heeftHuisarts;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification.heeftStatus;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification.isVolledig;
import static nl.rivm.screenit.util.StringUtil.propertyChain;

@Slf4j
@Service
public class CervixHuisartsServiceImpl implements CervixHuisartsService
{
	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private EnovationHuisartsService enovationHuisartsService;

	@Autowired
	private CervixHuisartsSyncService huisartsSyncService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private WoonplaatsService woonplaatsService;

	@Autowired
	private LogService logService;

	@Autowired
	private GebruikersService gebruikersService;

	@Autowired
	private OvereenkomstService overeenkomstService;

	@Autowired
	private CervixHuisartsRepository huisartsRepository;

	@Autowired
	private CervixHuisartsLocatieRepository huisartsLocatieRepository;

	@Autowired
	private CervixRegioBriefRepository regioBriefRepository;

	@Autowired
	private CervixLabformulierAanvraagRepository labformulierAanvraagRepository;

	@Override
	public CervixHuisarts getUitstrijkendArtsMetAgb(String agbCode)
	{
		var arts = getHuisartsMetAgbCode(agbCode);
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

			var enovationHuisarts = enovationHuisartsService.getHuisartsByAgb(agbCode);
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

				if (enovationHuisarts.getAdres() != null)
				{
					var postadresColonHuisarts = enovationHuisarts.getAdres();
					postadresCervixHuisarts.setWoonplaats(woonplaatsService.getWoonplaats(postadresColonHuisarts.getPlaats()));
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
	public CervixHuisarts getHuisartsMetAgbCode(String agb)
	{
		return huisartsRepository.findOneByAgbcode(agb).orElse(null);
	}

	@Override
	@Transactional
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
				huisartsSyncService.sendData(locatie);
			}
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

		huisartsSyncService.sendData(labformulierAanvraag);
	}

	@Override
	@Transactional
	public void saveOrUpdateArts(CervixHuisarts arts, LogGebeurtenis logGebeurtenis, InstellingGebruiker ingelogdeGebruiker)
	{
		updateGemeente(arts.getPostadres());
		var instellingGebruiker = arts.getOrganisatieMedewerkers().get(0);
		arts.setMutatiedatum(currentDateSupplier.getDate());
		arts.setNaam(huisartsSyncService.getPraktijkNaam(instellingGebruiker.getMedewerker()));
		hibernateService.saveOrUpdateAll(instellingGebruiker.getMedewerker(), arts);
		huisartsSyncService.sendData(arts);
		logService.logGebeurtenis(logGebeurtenis, ingelogdeGebruiker, "Huisarts: " + arts.getNaam(), Bevolkingsonderzoek.CERVIX);
	}

	@Override
	public CervixRegioBrief getLaatsteRegistratieBrief(CervixHuisarts arts)
	{
		return regioBriefRepository.findFirst(
			CervixRegioBriefSpecification.heeftHuisarts(arts).and(BriefSpecification.heeftBriefType(BriefType.REGIO_REGISTRATIE_UITSTRIJKEND_HUISARTS)),
			Sort.by(Sort.Order.desc(Brief_.CREATIE_DATUM))
		).orElse(null);
	}

	@Override
	@Transactional
	public void saveOrUpdateLocatie(CervixHuisartsLocatie locatie)
	{
		updateGemeente(locatie.getLocatieAdres());
		locatie.setMutatiedatum(currentDateSupplier.getDate());
		hibernateService.saveOrUpdateAll(locatie, locatie.getLocatieAdres());
		huisartsSyncService.sendData(locatie);
	}

	@Override
	public boolean isAndereLocatieMetNaam(CervixHuisartsLocatie locatie)
	{
		return huisartsLocatieRepository.exists(CervixHuisartsLocatieSpecification.heeftNaam(locatie.getNaam()).and(HibernateObjectSpecification.heeftNietId(locatie.getId()))
			.and(heeftHuisarts(locatie.getHuisarts())));
	}

	@Override
	@Transactional
	public void updateLabformulierAanvraag(CervixRegioMergedBrieven regioMergedBrieven)
	{
		var formulierAanvragen = getCervixLabformulierAanvraagFromMergedBrieven(regioMergedBrieven);
		if (!formulierAanvragen.isEmpty())
		{
			var isVolledigAfgedrukt = true;
			for (var aanvraag : formulierAanvragen)
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

					huisartsSyncService.sendData(aanvraag);
				}
			}
		}
	}

	private List<CervixLabformulierAanvraag> getCervixLabformulierAanvraagFromMergedBrieven(CervixRegioMergedBrieven regioMergedBrieven)
	{
		return labformulierAanvraagRepository.findAll(CervixRegioBriefSpecification.heeftMergedBrieven(regioMergedBrieven).with(CervixLabformulierAanvraag_.brief, JoinType.LEFT)
			.or(CervixRegioBriefSpecification.heeftMergedBrieven(regioMergedBrieven).with(CervixLabformulierAanvraag_.voorbladBrief, JoinType.LEFT)));
	}

	@Override
	public void resetWachtwoord(CervixHuisarts huisarts, Account loggedInAccount)
	{
		var huisartsID = huisarts.getScreenitId();

		var resetDto = new ResetDto();
		resetDto.setHuisarts_id(huisartsID);

		huisartsSyncService.sendData(resetDto);
		logService.logGebeurtenis(LogGebeurtenis.WACHTWOORD_GERESET, loggedInAccount, "Huisarts: " + huisarts.getNaam());
	}

	@Override
	@Transactional
	public void inactiveerHuisarts(CervixHuisarts huisarts, InstellingGebruiker loggedInAccount)
	{
		var nu = currentDateSupplier.getDate();
		huisarts.setActief(false);
		saveOrUpdateArts(huisarts, LogGebeurtenis.ORGANISATIE_INACTIVEERD, loggedInAccount);
		huisarts.getHuisartsLocaties().forEach(locatie -> inactiveerLocatie(locatie, nu, loggedInAccount));
	}

	@Override
	public List<CervixLabformulierAanvraag> getCervixLabformulierOrdersVanHuisarts(CervixHuisarts huisarts, long first, long count, Sort sort)
	{
		return labformulierAanvraagRepository.findWith(CervixLabformulierAanvraagSpecification.getAanvragenVanHuisarts(huisarts),
			q -> q.sortBy(sort, (order, r, cb) ->
			{
				var sortProperty = order.getProperty();
				if (sortProperty.startsWith(propertyChain(CervixLabformulierAanvraag_.INSTELLING_GEBRUIKER, InstellingGebruiker_.ORGANISATIE)))
				{
					var instellingGebruikerJoin = join(r, CervixLabformulierAanvraag_.instellingGebruiker);
					join(instellingGebruikerJoin, InstellingGebruiker_.organisatie);
				}
				return null;
			})).all(first, count);
	}

	@Override
	public long getAantalCervixLabformulierOrdersVanHuisarts(CervixHuisarts huisarts)
	{
		return labformulierAanvraagRepository.count(CervixLabformulierAanvraagSpecification.getAanvragenVanHuisarts(huisarts));
	}

	@Override
	public List<CervixHuisartsLocatie> getActieveHuisartsLocatiesVanHuisarts(CervixHuisarts huisarts)
	{
		return huisartsLocatieRepository.findAll(
			heeftHuisarts(huisarts).and(heeftStatus(CervixLocatieStatus.ACTIEF)).and(isVolledig()));
	}

	@Override
	public List<CervixHuisartsLocatie> getLocatiesVanHuisarts(CervixHuisartsLocatie zoekObject, long first, long count, Sort sort)
	{
		var specification = getLocatiesVanHuisartsSpecification(zoekObject);
		return huisartsLocatieRepository.findWith(specification, q -> q.sortBy(sort, (order, r, cb) ->
		{
			var sortProperty = order.getProperty();
			if (sortProperty.startsWith(CervixHuisartsLocatie_.LOCATIE_ADRES))
			{
				join(r, CervixHuisartsLocatie_.locatieAdres);
			}
			if (sortProperty.startsWith(propertyChain(CervixHuisartsLocatie_.LOCATIE_ADRES, CervixHuisartsAdres_.WOONPLAATS)))
			{
				join(join(r, CervixHuisartsLocatie_.locatieAdres), CervixHuisartsAdres_.woonplaats);
			}
			return null;
		})).all(first, count);
	}

	@Override
	public long getAantalLocatiesVanHuisarts(CervixHuisartsLocatie zoekObject)
	{
		var specification = getLocatiesVanHuisartsSpecification(zoekObject);
		return huisartsLocatieRepository.count(specification);
	}

	private ExtendedSpecification<CervixHuisartsLocatie> getLocatiesVanHuisartsSpecification(CervixHuisartsLocatie zoekObject)
	{
		var specification = heeftHuisarts(zoekObject.getHuisarts());
		var status = zoekObject.getStatus();
		if (status != null)
		{
			if (CervixLocatieStatus.INACTIEF.equals(status))
			{
				specification = specification.and(heeftStatus(CervixLocatieStatus.INACTIEF));
			}
			else
			{
				specification = specification.and(CervixHuisartsLocatieSpecification.heeftNietStatus(CervixLocatieStatus.INACTIEF));
			}
		}
		return specification;
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
		var aanvragen = labformulierAanvraagRepository.findByHuisartsLocatieAndStatus(locatie, CervixLabformulierAanvraagStatus.AANGEVRAAGD);
		aanvragen.forEach(aanvraag -> verwijderCervixLabformulierAanvraag(aanvraag, nu));
	}

	private void verwijderCervixLabformulierAanvraag(CervixLabformulierAanvraag aanvraag, Date nu)
	{
		aanvraag.setStatusDatum(nu);
		aanvraag.setStatus(CervixLabformulierAanvraagStatus.VERWIJDERD);
		hibernateService.saveOrUpdate(aanvraag);
		huisartsSyncService.sendData(aanvraag);
	}
}
