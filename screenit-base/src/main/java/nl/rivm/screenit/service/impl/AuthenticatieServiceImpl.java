package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.InstellingDao;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InlogStatus;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.util.CodeGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateSearchService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class AuthenticatieServiceImpl implements AuthenticatieService
{

	private static final Logger LOG = LoggerFactory.getLogger(AuthenticatieServiceImpl.class);

	@Autowired
	private HibernateSearchService hibernateSearchService;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private MailService mailService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private InstellingDao instellingDao;

	@Autowired
	@Qualifier(value = "applicationUrl")
	private String applicationUrl;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public Map<Gebruiker, Boolean> requestNewPassword(Gebruiker searchObject)
	{
		Gebruiker gebruiker = null;
		Map<Gebruiker, Boolean> gebruikerGelukt = new HashMap<Gebruiker, Boolean>();
		try
		{

			int size = hibernateSearchService.count(searchObject);

			if (size == 1)
			{
				gebruiker = hibernateSearchService.search(searchObject, -1, -1, "id", true).next();
			}

			if (gebruiker != null && gebruiker.getEmailextra() != null)
			{

				if (!gebruiker.getInlogMethode().equals(InlogMethode.UZIPAS))
				{
					String codeB = CodeGenerator.genereerCode(4, 5);
					gebruiker.setDatumWachtwoordAanvraag(currentDateSupplier.getDate());
					gebruiker.setWachtwoordChangeCode(codeB);
					String url = applicationUrl;
					if (!url.endsWith("/"))
					{
						url += "/";
					}
					String content = simplePreferenceService.getString(PreferenceKey.WACHTWOORDEMAIL.name(), "{link}");
					String link = "<a href=\"" + url + "passwordchange?code=" + codeB + "&user=" + gebruiker.getGebruikersnaam() + "\">Wachtwoord</a>";

					String aanhef = "";
					if (gebruiker.getAanhef() != null)
					{
						aanhef = " " + gebruiker.getAanhef().getNaam();
					}

					String titel = "";
					if (gebruiker.getTitel() != null)
					{
						titel = " " + gebruiker.getTitel().getNaam();
					}

					String achternaam = "";
					if (StringUtils.isNotBlank(gebruiker.getAchternaam()))
					{
						achternaam = " " + gebruiker.getAchternaam();
					}

					String tussenvoegsel = "";
					if (StringUtils.isNotBlank(gebruiker.getTussenvoegsel()))
					{
						tussenvoegsel = " " + gebruiker.getTussenvoegsel();
					}

					String voorletters = "";
					if (StringUtils.isNotBlank(gebruiker.getVoorletters()))
					{
						voorletters = " " + gebruiker.getVoorletters();
					}

					content = content.replaceAll("\\{link\\}", link);
					content = content.replaceAll("\\{aanhef\\}", aanhef);
					content = content.replaceAll("\\{titel\\}", titel);
					content = content.replaceAll("\\{achternaam\\}", achternaam);
					content = content.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
					content = content.replaceAll("\\{voorletters\\}", voorletters);
					content = content.replaceAll("\\{gebruikersnaam\\}", gebruiker.getGebruikersnaam());

					String emailAdres = gebruiker.getEmailwerk();
					if (StringUtils.isNotBlank(gebruiker.getEmailextra()))
					{
						emailAdres = gebruiker.getEmailextra();
					}

					boolean gelukt = false;

					gelukt = mailService.sendEmail(emailAdres, simplePreferenceService.getString(PreferenceKey.WACHTWOORDEMAILSUBJECT.name(), "Geen onderwerp"), content);

					gebruikerGelukt.put(gebruiker, gelukt);
				}
			}
			else
			{
				gebruikerGelukt.put(gebruiker, false);
			}
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
		return gebruikerGelukt;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public Map<Gebruiker, Boolean> accountGeblokkeerd(Gebruiker gebruiker)
	{
		Map<Gebruiker, Boolean> gebruikerGelukt = new HashMap<Gebruiker, Boolean>();
		try
		{

			if (gebruiker != null && gebruiker.getEmailextra() != null)
			{
				String content = simplePreferenceService.getString(PreferenceKey.GEBLOKKEERDEMAIL.name(), "U account is geblokkeerd door een beheerder.");

				String aanhef = "";
				if (gebruiker.getAanhef() != null)
				{
					aanhef = " " + gebruiker.getAanhef().getNaam();
				}

				String titel = "";
				if (gebruiker.getTitel() != null)
				{
					titel = " " + gebruiker.getTitel().getNaam();
				}

				String achternaam = "";
				if (StringUtils.isNotBlank(gebruiker.getAchternaam()))
				{
					achternaam = " " + gebruiker.getAchternaam();
				}

				String tussenvoegsel = "";
				if (StringUtils.isNotBlank(gebruiker.getTussenvoegsel()))
				{
					tussenvoegsel = " " + gebruiker.getTussenvoegsel();
				}

				String voorletters = "";
				if (StringUtils.isNotBlank(gebruiker.getVoorletters()))
				{
					voorletters = " " + gebruiker.getVoorletters();
				}

				content = content.replaceAll("\\{aanhef\\}", aanhef);
				content = content.replaceAll("\\{titel\\}", titel);
				content = content.replaceAll("\\{achternaam\\}", achternaam);
				content = content.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
				content = content.replaceAll("\\{voorletters\\}", voorletters);
				content = content.replaceAll("\\{gebruikersnaam\\}", gebruiker.getGebruikersnaam());

				String emailAdres = gebruiker.getEmailwerk();
				if (StringUtils.isNotBlank(gebruiker.getEmailextra()))
				{
					emailAdres = gebruiker.getEmailextra();
				}

				boolean gelukt = false;

				gelukt = mailService.sendEmail(emailAdres, simplePreferenceService.getString(PreferenceKey.GEBLOKKEERDEMAILSUBJECT.name(), "Account geblokkeerd"), content);

				gebruikerGelukt.put(gebruiker, gelukt);
			}
			else
			{
				gebruikerGelukt.put(gebruiker, false);
			}
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
		return gebruikerGelukt;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public void sendUziEmail(Gebruiker gebruiker)
	{
		if (gebruiker.getEmailextra() != null)
		{
			String content = simplePreferenceService.getString(PreferenceKey.UZIEMAIL.name(), "Het Uzinummer is toegevoegd.");

			String aanhef = "";
			if (gebruiker.getAanhef() != null)
			{
				aanhef = " " + gebruiker.getAanhef().getNaam();
			}

			String titel = "";
			if (gebruiker.getTitel() != null)
			{
				titel = " " + gebruiker.getTitel().getNaam();
			}

			String achternaam = "";
			if (StringUtils.isNotBlank(gebruiker.getAchternaam()))
			{
				achternaam = " " + gebruiker.getAchternaam();
			}

			String tussenvoegsel = "";
			if (StringUtils.isNotBlank(gebruiker.getTussenvoegsel()))
			{
				tussenvoegsel = " " + gebruiker.getTussenvoegsel();
			}

			String voorletters = "";
			if (StringUtils.isNotBlank(gebruiker.getVoorletters()))
			{
				voorletters = " " + gebruiker.getVoorletters();
			}

			content = content.replaceAll("\\{uzinummer\\}", gebruiker.getUzinummer());
			content = content.replaceAll("\\{aanhef\\}", aanhef);
			content = content.replaceAll("\\{titel\\}", titel);
			content = content.replaceAll("\\{achternaam\\}", achternaam);
			content = content.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
			content = content.replaceAll("\\{voorletters\\}", voorletters);
			content = content.replaceAll("\\{gebruikersnaam\\}", gebruiker.getGebruikersnaam());

			String emailAdres = gebruiker.getEmailwerk();
			if (StringUtils.isNotBlank(gebruiker.getEmailextra()))
			{
				emailAdres = gebruiker.getEmailextra();
			}

			mailService.sendEmail(emailAdres, simplePreferenceService.getString(PreferenceKey.UZIEMAILSUBJECT.name(), "Geen onderwerp"), content);
		}

	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public List<InstellingGebruiker> getActieveInstellingGebruikers(Gebruiker gebruiker)
	{
		return instellingDao.getInstellingGebruikersVoorInloggen(gebruiker);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public Boolean isAccountLocked(Gebruiker gebruiker)
	{
		Integer foutieveAanmeldpogingenTimeout = simplePreferenceService.getInteger(PreferenceKey.FOUTIEVE_AANMELDPOGINGEN_TIMEOUT.name());
		if (foutieveAanmeldpogingenTimeout == null)
		{
			foutieveAanmeldpogingenTimeout = Integer.valueOf(30);
		}
		return gebruiker != null && 
			(InlogStatus.GEBLOKKEERD.equals(gebruiker.getInlogstatus()) || 
				(InlogStatus.TIJDELIJK_GEBLOKKEERD.equals(gebruiker.getInlogstatus()) 
					&& new DateTime(gebruiker.getTijdLaatsteFoutieveInlog()).plusMinutes(foutieveAanmeldpogingenTimeout).isAfterNow()));
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void unlockAccount(Gebruiker gebruiker)
	{

		gebruiker.setFoutieveInlogpogingen(Integer.valueOf(0));
		gebruiker.setTijdLaatsteFoutieveInlog(null);
		gebruiker.setInlogstatus(InlogStatus.OK);
		hibernateService.saveOrUpdate(gebruiker);
	}

	@Override
	public void foutieveInlogpoging(Gebruiker gebruiker)
	{
		if (gebruiker != null && InlogStatus.OK.equals(gebruiker.getInlogstatus()))
		{
			Integer foutieveAanmeldpogingenTimeout = simplePreferenceService.getInteger(PreferenceKey.FOUTIEVE_AANMELDPOGINGEN_TIMEOUT.name());
			if (foutieveAanmeldpogingenTimeout == null)
			{
				foutieveAanmeldpogingenTimeout = Integer.valueOf(30);
			}
			Integer maxFoutieveAanmeldpogingen = simplePreferenceService.getInteger(PreferenceKey.MAXIMUM_FOUTIEVE_AANMELDPOGINGEN.name());
			if (maxFoutieveAanmeldpogingen == null)
			{
				maxFoutieveAanmeldpogingen = Integer.valueOf(3);
			}
			Integer foutieveInlogpogingen = gebruiker.getFoutieveInlogpogingen();
			if (foutieveInlogpogingen == null)
			{
				foutieveInlogpogingen = Integer.valueOf(1);
			}
			else
			{
				foutieveInlogpogingen++;
			}
			gebruiker.setFoutieveInlogpogingen(foutieveInlogpogingen);

			if (gebruiker.getTijdLaatsteFoutieveInlog() == null)
			{
				gebruiker.setTijdLaatsteFoutieveInlog(currentDateSupplier.getDate());
			}
			long diff = currentDateSupplier.getDate().getTime() - gebruiker.getTijdLaatsteFoutieveInlog().getTime();
			boolean blokkeren = foutieveInlogpogingen >= maxFoutieveAanmeldpogingen && diff < 60000 * foutieveAanmeldpogingenTimeout;
			if (blokkeren)
			{
				gebruiker.setInlogstatus(InlogStatus.TIJDELIJK_GEBLOKKEERD);
			}

			gebruiker.setTijdLaatsteFoutieveInlog(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(gebruiker);
		}
	}

}
