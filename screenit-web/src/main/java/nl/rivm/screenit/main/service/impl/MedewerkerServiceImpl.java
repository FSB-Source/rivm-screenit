package nl.rivm.screenit.main.service.impl;

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
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.dao.MedewerkerDao;
import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class MedewerkerServiceImpl implements MedewerkerService
{

	@Autowired
	private MedewerkerDao medewerkerDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private AuthenticatieService authenticatieService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private MailService mailService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Override
	public List<Gebruiker> searchMedewerkers(Gebruiker searchObject, List<Functie> selectedFuncties, List<Rol> selectedRollen,
		Map<OrganisatieType, List<Instelling>> hierarchieCriteria, List<Bevolkingsonderzoek> bevolkingsonderzoeken, int first, int count, String sortProperty, boolean ascending)
	{
		return medewerkerDao.searchMedewerkers(searchObject, selectedFuncties, selectedRollen, hierarchieCriteria, bevolkingsonderzoeken, first, count, sortProperty, ascending);
	}

	@Override
	public long countMedewerkers(Gebruiker searchObject, List<Functie> selectedFuncties, List<Rol> selectedRollen, Map<OrganisatieType, List<Instelling>> hierarchieCriteria,
		List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		return medewerkerDao.countMedewerkers(searchObject, selectedFuncties, selectedRollen, hierarchieCriteria, bevolkingsonderzoeken);
	}

	@Override
	public List<InstellingGebruiker> searchInstellingGebruiker(InstellingGebruiker zoekInstellingGebruiker, long first, long count, String orderByProperty, boolean ascending)
	{
		return medewerkerDao.searchInstellingGebruiker(zoekInstellingGebruiker, first, count, orderByProperty, ascending);
	}

	@Override
	public List<InstellingGebruiker> getActieveRadiologen(InstellingGebruiker zoekInstellingGebruiker, List<Long> exclIds, String orderByProperty, boolean ascending)
	{
		return medewerkerDao.getActieveRadiologen(zoekInstellingGebruiker, exclIds, orderByProperty, ascending);
	}

	@Override
	public long countInstellingGebruiker(InstellingGebruiker instellingGebruiker)
	{
		return medewerkerDao.countInstellingGebruiker(instellingGebruiker);
	}

	@Override
	public void addOrganisatieMedewerker(Instelling organisatie, Gebruiker medewerker)
	{

		var organisatieMedewerker = medewerkerDao.getInstellingGebruiker(organisatie, medewerker);

		if (organisatieMedewerker != null)
		{

			if (Boolean.FALSE.equals(organisatieMedewerker.getActief()))
			{
				organisatieMedewerker.setActief(Boolean.TRUE);
				medewerkerDao.saveOrUpdateInstellingGebruiker(organisatieMedewerker);
			}
		}
		else
		{

			organisatieMedewerker = new InstellingGebruiker();
			organisatieMedewerker.setActief(Boolean.TRUE);
			organisatieMedewerker.setOrganisatie(organisatie);
			organisatieMedewerker.setMedewerker(medewerker);
			organisatieMedewerker.setRollen(new ArrayList<InstellingGebruikerRol>());
			if (medewerker.getOrganisatieMedewerkers() == null)
			{
				medewerker.setOrganisatieMedewerkers(new ArrayList<InstellingGebruiker>());
			}
			medewerker.getOrganisatieMedewerkers().add(organisatieMedewerker);
			if (organisatie.getOrganisatieMedewerkers() == null)
			{
				organisatie.setOrganisatieMedewerkers(new ArrayList<InstellingGebruiker>());
			}
			organisatie.getOrganisatieMedewerkers().add(organisatieMedewerker);
			medewerkerDao.saveOrUpdateInstellingGebruiker(organisatieMedewerker);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateRollen(InstellingGebruiker instellingGebruiker)
	{
		medewerkerDao.saveOrUpdateInstellingGebruiker(instellingGebruiker);
		for (var rol : instellingGebruiker.getRollen())
		{
			hibernateService.saveOrUpdate(rol);
		}

	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean saveOrUpdateGebruiker(Gebruiker medewerker, boolean isBestaande, boolean wordGeblokkeerd)
	{
		var gelukt = true;
		var handtekening = medewerker.getHandtekening();
		if (handtekening != null && !handtekening.getActief())
		{
			medewerker.setHandtekening(null);
			if (handtekening.getId() != null)
			{
				uploadDocumentService.delete(handtekening, true);
			}
		}
		if (isBestaande)
		{
			hibernateService.saveOrUpdate(medewerker);
			if (wordGeblokkeerd)
			{
				authenticatieService.accountGeblokkeerd(medewerker);
			}
		}
		else
		{
			if (medewerker.getInlogMethode().equals(InlogMethode.UZIPAS))
			{
				hibernateService.saveOrUpdate(medewerker);
				if (medewerker.getUzinummer() != null)
				{
					authenticatieService.sendUziEmail(medewerker);
				}
			}
			else
			{
				gelukt = resetWachtwoord(medewerker);
			}
		}
		return gelukt;

	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void inActiveerGebruiker(Gebruiker medewerker)
	{
		medewerker.setActief(Boolean.FALSE.equals(medewerker.getActief()));
		hibernateService.saveOrUpdate(medewerker);
		if (Boolean.FALSE.equals(medewerker.getActief()))
		{
			if (StringUtils.isNotBlank(medewerker.getEmailextra()))
			{
				var inactiverenemail = preferenceService.getString(PreferenceKey.INACTIVERENEMAIL.name(), "Beste gebruiker, <br><br>"
					+ "U gebruiker account met de gebruikersnaam '{gebruikersnaam}' is ge&iuml;nactiveerd." + " <br><br>Met vriendelijke groeten, <br>Het ScreenIT team");
				inactiverenemail = inactiverenemail.replaceAll("\\{gebruikersnaam\\}", medewerker.getGebruikersnaam());
				var aanhef = "";
				if (medewerker.getAanhef() != null)
				{
					aanhef = " " + medewerker.getAanhef().getNaam();
				}

				var titel = "";
				if (medewerker.getTitel() != null)
				{
					titel = " " + medewerker.getTitel().getNaam();
				}

				var achternaam = "";
				if (StringUtils.isNotBlank(medewerker.getAchternaam()))
				{
					achternaam = " " + medewerker.getAchternaam();
				}

				var tussenvoegsel = "";
				if (StringUtils.isNotBlank(medewerker.getTussenvoegsel()))
				{
					tussenvoegsel = " " + medewerker.getTussenvoegsel();
				}

				var voorletters = "";
				if (StringUtils.isNotBlank(medewerker.getVoorletters()))
				{
					voorletters = " " + medewerker.getVoorletters();
				}
				inactiverenemail = inactiverenemail.replaceAll("\\{aanhef\\}", aanhef);
				inactiverenemail = inactiverenemail.replaceAll("\\{titel\\}", titel);
				inactiverenemail = inactiverenemail.replaceAll("\\{achternaam\\}", achternaam);
				inactiverenemail = inactiverenemail.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
				inactiverenemail = inactiverenemail.replaceAll("\\{voorletters\\}", voorletters);
				var inactiverensubject = preferenceService.getString(PreferenceKey.INACTIVERENSUBJECT.name(), "ScreenIT - Gebruiker account ge\u00EFnactiveerd");
				mailService.queueMail(medewerker.getEmailextra(), inactiverensubject, inactiverenemail);
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public boolean resetWachtwoord(Gebruiker medewerker)
	{
		medewerker.setWachtwoord(null);
		hibernateService.saveOrUpdate(medewerker);
		var requestMedewerker = new Gebruiker();
		requestMedewerker.setGebruikersnaam(medewerker.getGebruikersnaam());
		var medewerkerMap = authenticatieService.requestNewPassword(requestMedewerker);
		var gelukt = false;

		medewerker = null;
		if (medewerkerMap.size() > 0)
		{
			var entry = medewerkerMap.entrySet().iterator().next();
			medewerker = entry.getKey();
			gelukt = !Boolean.FALSE.equals(entry.getValue());
		}

		if (medewerker != null)
		{
			hibernateService.saveOrUpdate(medewerker);
		}

		return gelukt;
	}

	@Override
	public List<Gebruiker> getActieveGebruikersMetRecht(Recht recht)
	{
		return medewerkerDao.getActieveGebruikersMetRecht(recht);
	}

	@Override
	public List<InstellingGebruikerRol> getInstellingGebruikersMetRolEnBvos(Rol rol, List<Bevolkingsonderzoek> onderzoeken)
	{
		var rollen = new ArrayList<InstellingGebruikerRol>();
		var instellingGebruikersRollen = medewerkerDao.getInstellingGebruikersRollenMetRol(rol, currentDateSupplier.getDate());
		if (onderzoeken == null)
		{
			rollen.addAll(instellingGebruikersRollen);
		}
		else
		{
			for (var igRol : instellingGebruikersRollen)
			{
				for (var verwijderdeOnderzoek : onderzoeken)
				{
					if (igRol.getBevolkingsonderzoeken().contains(verwijderdeOnderzoek))
					{
						rollen.add(igRol);
						break;
					}
				}
			}
		}
		return rollen;
	}

	@Override
	public boolean zijnErInstellingGebruikersMetRol(Rol rol)
	{
		return !getInstellingGebruikersMetRolEnBvos(rol, null).isEmpty();
	}

}
