package nl.rivm.screenit.service.impl;

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

import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InlogStatus;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.repository.algemeen.GebruikerRepository;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.util.CodeGenerator;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
public class AuthenticatieServiceImpl implements AuthenticatieService
{

	@Autowired
	private GebruikerRepository gebruikerRepository;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private MailService mailService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private InstellingService instellingService;

	@Autowired
	@Qualifier(value = "applicationUrl")
	private String applicationUrl;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Gebruiker requestNewPassword(String gebruikersnaam, String emailextra)
	{
		Gebruiker gebruiker = null;
		try
		{
			if (StringUtils.isNotBlank(gebruikersnaam))
			{
				gebruiker = gebruikerRepository.findByGebruikersnaam(gebruikersnaam).orElse(null);
			}
			else if (StringUtils.isNotBlank(emailextra))
			{
				gebruiker = gebruikerRepository.findByEmailextraAndActief(emailextra, true).orElse(null);
			}

			if (gebruiker != null && gebruiker.getEmailextra() != null && gebruiker.getInlogMethode() != InlogMethode.UZIPAS)
			{
				var codeB = CodeGenerator.genereerCode(4, 5);
				gebruiker.setDatumWachtwoordAanvraag(currentDateSupplier.getDate());
				gebruiker.setWachtwoordChangeCode(codeB);
				var url = applicationUrl;
				if (!url.endsWith("/"))
				{
					url += "/";
				}
				var content = simplePreferenceService.getString(PreferenceKey.WACHTWOORDEMAIL.name(), "{link}");
				var link = "<a href=\"" + url + "passwordchange?code=" + codeB + "&user=" + gebruiker.getGebruikersnaam() + "\">Wachtwoord</a>";

				var aanhef = "";
				if (gebruiker.getAanhef() != null)
				{
					aanhef = " " + gebruiker.getAanhef().getNaam();
				}

				var titel = "";
				if (gebruiker.getTitel() != null)
				{
					titel = " " + gebruiker.getTitel().getNaam();
				}

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

				content = content.replaceAll("\\{link\\}", link);
				content = content.replaceAll("\\{aanhef\\}", aanhef);
				content = content.replaceAll("\\{titel\\}", titel);
				content = content.replaceAll("\\{achternaam\\}", achternaam);
				content = content.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
				content = content.replaceAll("\\{voorletters\\}", voorletters);
				content = content.replaceAll("\\{gebruikersnaam\\}", gebruiker.getGebruikersnaam());

				var emailAdres = gebruiker.getEmailextra();

				mailService.queueMailAanProfessional(emailAdres, simplePreferenceService.getString(PreferenceKey.WACHTWOORDEMAILSUBJECT.name(), "Geen onderwerp"), content);
			}
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
		return gebruiker;
	}

	@Override
	public Map<Gebruiker, Boolean> accountGeblokkeerd(Gebruiker gebruiker)
	{
		var gebruikerGelukt = new HashMap<Gebruiker, Boolean>();
		try
		{

			if (gebruiker != null && gebruiker.getEmailextra() != null)
			{
				var content = simplePreferenceService.getString(PreferenceKey.GEBLOKKEERDEMAIL.name(), "U account is geblokkeerd door een beheerder.");

				var aanhef = "";
				if (gebruiker.getAanhef() != null)
				{
					aanhef = " " + gebruiker.getAanhef().getNaam();
				}

				var titel = "";
				if (gebruiker.getTitel() != null)
				{
					titel = " " + gebruiker.getTitel().getNaam();
				}

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

				content = content.replaceAll("\\{aanhef\\}", aanhef);
				content = content.replaceAll("\\{titel\\}", titel);
				content = content.replaceAll("\\{achternaam\\}", achternaam);
				content = content.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
				content = content.replaceAll("\\{voorletters\\}", voorletters);
				content = content.replaceAll("\\{gebruikersnaam\\}", gebruiker.getGebruikersnaam());

				var emailAdres = gebruiker.getEmailwerk();
				if (StringUtils.isNotBlank(gebruiker.getEmailextra()))
				{
					emailAdres = gebruiker.getEmailextra();
				}

				mailService.queueMailAanProfessional(emailAdres, simplePreferenceService.getString(PreferenceKey.GEBLOKKEERDEMAILSUBJECT.name(), "Account geblokkeerd"), content);
				gebruikerGelukt.put(gebruiker, true);
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
	public void sendUziEmail(Gebruiker gebruiker)
	{
		if (gebruiker.getEmailextra() != null)
		{
			var content = simplePreferenceService.getString(PreferenceKey.UZIEMAIL.name(), "Het Uzinummer is toegevoegd.");

			var aanhef = "";
			if (gebruiker.getAanhef() != null)
			{
				aanhef = " " + gebruiker.getAanhef().getNaam();
			}

			var titel = "";
			if (gebruiker.getTitel() != null)
			{
				titel = " " + gebruiker.getTitel().getNaam();
			}

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

			content = content.replaceAll("\\{uzinummer\\}", gebruiker.getUzinummer());
			content = content.replaceAll("\\{aanhef\\}", aanhef);
			content = content.replaceAll("\\{titel\\}", titel);
			content = content.replaceAll("\\{achternaam\\}", achternaam);
			content = content.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
			content = content.replaceAll("\\{voorletters\\}", voorletters);
			content = content.replaceAll("\\{gebruikersnaam\\}", gebruiker.getGebruikersnaam());

			var emailAdres = gebruiker.getEmailwerk();
			if (StringUtils.isNotBlank(gebruiker.getEmailextra()))
			{
				emailAdres = gebruiker.getEmailextra();
			}

			mailService.queueMailAanProfessional(emailAdres, simplePreferenceService.getString(PreferenceKey.UZIEMAILSUBJECT.name(), "Geen onderwerp"), content);
		}

	}

	@Override
	public List<InstellingGebruiker> getActieveInstellingGebruikers(Gebruiker gebruiker)
	{
		return instellingService.getActieveInstellingGebruikersMetRollen(gebruiker);
	}

	@Override
	public Boolean isAccountLocked(Gebruiker gebruiker)
	{
		var foutieveAanmeldpogingenTimeout = simplePreferenceService.getInteger(PreferenceKey.FOUTIEVE_AANMELDPOGINGEN_TIMEOUT.name());
		if (foutieveAanmeldpogingenTimeout == null)
		{
			foutieveAanmeldpogingenTimeout = 30;
		}
		return gebruiker != null &&
			(InlogStatus.GEBLOKKEERD.equals(gebruiker.getInlogstatus()) ||
				(InlogStatus.TIJDELIJK_GEBLOKKEERD.equals(gebruiker.getInlogstatus())
					&& DateUtil.compareAfter(DateUtil.plusTijdseenheid(gebruiker.getTijdLaatsteFoutieveInlog(), foutieveAanmeldpogingenTimeout, ChronoUnit.MINUTES),
					currentDateSupplier.getDate())));
	}

	@Override
	@Transactional
	public void unlockAccount(Gebruiker gebruiker)
	{

		gebruiker.setFoutieveInlogpogingen(0);
		gebruiker.setTijdLaatsteFoutieveInlog(null);
		gebruiker.setInlogstatus(InlogStatus.OK);
		hibernateService.saveOrUpdate(gebruiker);
	}

	@Override
	public void foutieveInlogpoging(Gebruiker gebruiker)
	{
		if (gebruiker != null && InlogStatus.OK.equals(gebruiker.getInlogstatus()))
		{
			var foutieveAanmeldpogingenTimeout = simplePreferenceService.getInteger(PreferenceKey.FOUTIEVE_AANMELDPOGINGEN_TIMEOUT.name());
			if (foutieveAanmeldpogingenTimeout == null)
			{
				foutieveAanmeldpogingenTimeout = 30;
			}
			var maxFoutieveAanmeldpogingen = simplePreferenceService.getInteger(PreferenceKey.MAXIMUM_FOUTIEVE_AANMELDPOGINGEN.name());
			if (maxFoutieveAanmeldpogingen == null)
			{
				maxFoutieveAanmeldpogingen = 3;
			}
			var foutieveInlogpogingen = gebruiker.getFoutieveInlogpogingen();
			if (foutieveInlogpogingen == null)
			{
				foutieveInlogpogingen = 1;
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
			var diff = currentDateSupplier.getDate().getTime() - gebruiker.getTijdLaatsteFoutieveInlog().getTime();
			var blokkeren = foutieveInlogpogingen >= maxFoutieveAanmeldpogingen && diff < 60000L * foutieveAanmeldpogingenTimeout;
			if (blokkeren)
			{
				gebruiker.setInlogstatus(InlogStatus.TIJDELIJK_GEBLOKKEERD);
			}

			gebruiker.setTijdLaatsteFoutieveInlog(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(gebruiker);
		}
	}

}
