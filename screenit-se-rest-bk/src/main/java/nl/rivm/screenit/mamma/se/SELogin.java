package nl.rivm.screenit.mamma.se;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDateTime;
import java.util.List;
import java.util.Locale;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.mamma.se.dto.LoginDto;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidService;
import nl.rivm.screenit.mamma.se.service.SELogService;
import nl.rivm.screenit.mamma.se.service.SeAutorisatieService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InlogStatus;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.security.InstellingGebruikerToken;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.rivm.screenit.service.GebruikersService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.MedewerkerUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.Organisatie;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;
import nl.topicuszorg.yubikey.shiro.YubikeyToken;

import org.apache.shiro.SecurityUtils;
import org.apache.shiro.authc.AuthenticationException;
import org.apache.shiro.authc.IncorrectCredentialsException;
import org.apache.shiro.authc.UnknownAccountException;
import org.apache.shiro.authc.UsernamePasswordToken;
import org.apache.shiro.mgt.SecurityManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SELogin
{
	private static final Logger LOG = LoggerFactory.getLogger(SELogin.class);

	private static final String DEFAULT_MESSAGE = "Aanmelden mislukt, mogelijk zijn de ingevoerde gegevens niet correct.";

	private static final String GEEN_OTP = "GEEN_OTP";

	private SimplePreferenceService preferenceService;

	private AuthenticatieService authenticatieService;

	private SeAutorisatieService seAutorisatieService;

	private MammaScreeningsEenheidService screeningseenhedenService;

	private HibernateService hibernateService;

	private GebruikersService gebruikerService;

	private SELogService logService;

	private String applicationEnvironment;

	private ICurrentDateSupplier currentDateSupplier;

	private Long screeningsEenheidId;

	private Long accountId;

	public SELogin()
	{
		preferenceService = SpringBeanProvider.getInstance().getBean(SimplePreferenceService.class);
		authenticatieService = SpringBeanProvider.getInstance().getBean(AuthenticatieService.class);
		seAutorisatieService = SpringBeanProvider.getInstance().getBean(SeAutorisatieService.class);
		screeningseenhedenService = SpringBeanProvider.getInstance().getBean(MammaScreeningsEenheidService.class);
		hibernateService = SpringBeanProvider.getInstance().getBean(HibernateService.class);
		gebruikerService = SpringBeanProvider.getInstance().getBean(GebruikersService.class);
		logService = SpringBeanProvider.getInstance().getBean(SELogService.class);
		applicationEnvironment = SpringBeanProvider.getInstance().getBean(String.class, "applicationEnvironment");
		currentDateSupplier = SpringBeanProvider.getInstance().getBean(ICurrentDateSupplier.class);
		Locale.setDefault(Constants.LOCALE_NL);
	}

	public LoginDto doLogin(String seCode, LocalDateTime proxyDatumTijd, String gebruikersnaam, String plainWachtwoord, String yubikey, String versie, String nfcServerVersie,
		boolean genereerLoggebeurtenis)
	{
		SecurityManager securityManager = SecurityUtils.getSecurityManager();
		LoginDto result = new LoginDto(false);

		MammaScreeningsEenheid screeningsEenheid = screeningseenhedenService.getActieveScreeningsEenheidByCode(seCode);
		if (screeningsEenheid == null)
		{
			LOG.error("Screeningseenheid niet herkend! SE-code: " + seCode);
			result.setMessage(DEFAULT_MESSAGE);
			return result;
		}
		screeningsEenheidId = screeningsEenheid.getId();
		Gebruiker gebruiker = null;

		UsernamePasswordToken token = yubikey.equals(GEEN_OTP) ? new UsernamePasswordToken(gebruikersnaam, plainWachtwoord)
			: new YubikeyToken(gebruikersnaam, plainWachtwoord, yubikey);

		try
		{
			gebruiker = gebruikerService.getGebruikerByGebruikersnaam(token.getUsername());

			if (gebruiker == null)
			{
				logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, null, screeningsEenheid, proxyDatumTijd, "Onbekende gebruikersnaam");
				result.setMessage(DEFAULT_MESSAGE);
			}
			else if (token instanceof YubikeyToken && gebruiker.getInlogMethode() != InlogMethode.YUBIKEY)
			{
				LOG.error("Gebruiker probeert in te loggen met Yubikey, maar inlogmethode is: {}", gebruiker.getInlogMethode());
				result.setMessage(DEFAULT_MESSAGE);
			}
			else if (!(token instanceof YubikeyToken) && gebruiker.getInlogMethode() != InlogMethode.GEBRUIKERSNAAM_WACHTWOORD)
			{
				LOG.error("Gebruiker probeert in te loggen met Gebruikersnaam/ww, maar inlogmethode is: {}", gebruiker.getInlogMethode());
				result.setMessage(DEFAULT_MESSAGE);
			}
			else if (!(token instanceof YubikeyToken) && applicationEnvironment.equalsIgnoreCase("PRODUCTIE"))
			{
				LOG.error("Gebruiker probeert in te loggen met Gebruikersnaam/ww in productie, inlogmethode bij gebruiker: {}", gebruiker.getInlogMethode());
				result.setMessage(DEFAULT_MESSAGE);
			}
			else
			{
				securityManager.authenticate(token);
				String meldingNavActiefVanafEnTotEnMet = MedewerkerUtil.meldingNavActiefVanafEnTotEnMet(gebruiker, currentDateSupplier.getDate());
				if (meldingNavActiefVanafEnTotEnMet != null)
				{
					result.setMessage(meldingNavActiefVanafEnTotEnMet);
				}
				else
				{
					result = login(screeningsEenheid, proxyDatumTijd, gebruiker, versie, nfcServerVersie, genereerLoggebeurtenis);
				}
			}
		}
		catch (UnknownAccountException | IncorrectCredentialsException ice)
		{
			authenticatieService.foutieveInlogpoging(gebruiker);
			logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, gebruiker, screeningsEenheid, proxyDatumTijd,
				"Verkeerde credentials of herhalend gebruik van dezelfde Yubikey code");
			result.setMessage(getError(gebruiker, screeningsEenheid, proxyDatumTijd));
		}
		catch (AuthenticationException authExcept)
		{
			LOG.error("Onbekende fout bij inloggen (is het wachtwoord ingevuld?)", authExcept);
		}

		return result;
	}

	private LoginDto login(MammaScreeningsEenheid screeningsEenheid, LocalDateTime proxyDatumTijd, Gebruiker gebruiker, String versie, String nfcServerVersie,
		boolean genereerLoggebeurtenis)
	{
		LoginDto result = new LoginDto(false);

		if (!authenticatieService.isAccountLocked(gebruiker))
		{
			authenticatieService.unlockAccount(gebruiker);

			List<InstellingGebruiker> instellingGebruikers = authenticatieService.getActieveInstellingGebruikers(gebruiker);
			if (instellingGebruikers.size() == 0)
			{

				logGeenActieveInstelling(gebruiker, screeningsEenheid, proxyDatumTijd);
				result.setMessage("Er zijn geen actieve instellingen gekoppeld aan deze gebruiker.");
				LOG.error(result.getMessage());
			}
			else
			{
				InstellingGebruiker inlogInstellingGebruiker = getInstellingGebruiker(instellingGebruikers, screeningsEenheid);
				if (inlogInstellingGebruiker != null)
				{

					InstellingGebruikerToken token = new InstellingGebruikerToken(inlogInstellingGebruiker.getId());
					SecurityUtils.getSubject().login(token);
					if (seAutorisatieService.isGeautoriseerdVoorInloggen(inlogInstellingGebruiker.getId()))
					{
						accountId = inlogInstellingGebruiker.getId();
						result.setSuccess(true);
						String seVersie = versie == null ? "onbekend" : versie;
						String nfcVersie = nfcServerVersie == null || nfcServerVersie.equals("undefined") ? "onbekend" : nfcServerVersie;
						String logBericht = String.format("SE-Proxy versie: %s, Nfc webserver versie: %s", seVersie, nfcVersie);
						if (genereerLoggebeurtenis)
						{
							logService.logInfo(LogGebeurtenis.INLOGGEN, inlogInstellingGebruiker, screeningsEenheid, proxyDatumTijd, logBericht);
						}
						else
						{
							LOG.info("Sessie meenemen {} {} {}", seLogCode(screeningsEenheid), accountIdLogTekst(inlogInstellingGebruiker), logBericht);
						}
					}
					else
					{
						logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, inlogInstellingGebruiker, screeningsEenheid, proxyDatumTijd,
							"Gebruiker heeft niet het Inschrijven op SE recht om in te loggen.");
						SecurityUtils.getSubject().logout();
						result.setSuccess(false);
						result.setMessage("Inloggen is mislukt. Het recht Inschrijven op SE ontbreekt.");
					}
				}
				else
				{
					logGeenActieveInstelling(gebruiker, screeningsEenheid, proxyDatumTijd);
					result.setMessage("Inloggen mislukt. Gebruiker is niet gekoppeld aan de Screeningsorganisatie "
						+ screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio().getNaam());
				}
			}
		}
		else
		{
			result.setMessage(getError(gebruiker, screeningsEenheid, proxyDatumTijd));
		}
		return result;
	}

	private String seLogCode(MammaScreeningsEenheid screeningsEenheid)
	{
		return screeningsEenheid == null ? "SE-???" : screeningsEenheid.getCode();
	}

	private void logGeenActieveInstelling(Gebruiker gebruiker, MammaScreeningsEenheid screeningsEenheid, LocalDateTime proxyDatumTijd)
	{
		logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, gebruiker, screeningsEenheid, proxyDatumTijd,
			"Gebruikersnaam: " + gebruiker.getGebruikersnaam()
				+ ", geen actieve instelling gekoppeld aan medewerker of niet voldoende rechten voor gekoppelde instelling(en).");
	}

	private InstellingGebruiker getInstellingGebruiker(List<InstellingGebruiker> instellingGebruikers, MammaScreeningsEenheid inlogScreeningsEenheid)
	{
		InstellingGebruiker inlogInstellingGebruiker = null;
		for (InstellingGebruiker instellingGebruiker : instellingGebruikers)
		{

			Organisatie organisatie = instellingGebruiker.getOrganisatie(); 
			if (organisatie instanceof ScreeningOrganisatie)
			{
				BeoordelingsEenheid be = inlogScreeningsEenheid.getBeoordelingsEenheid();
				Instelling ce = be.getParent();
				Instelling so = ce.getRegio();
				if (so.equals(organisatie))
				{
					inlogInstellingGebruiker = instellingGebruiker;
				}
			}
		}
		return inlogInstellingGebruiker;
	}

	private String getError(Gebruiker inTeLoggenMedewerker, MammaScreeningsEenheid screeningsEenheid, LocalDateTime proxyDatumTijd)
	{
		Integer foutieveAanmeldpogingenTimeout = preferenceService.getInteger(PreferenceKey.FOUTIEVE_AANMELDPOGINGEN_TIMEOUT.name());
		String resultMessage = "";
		if (foutieveAanmeldpogingenTimeout == null)
		{
			foutieveAanmeldpogingenTimeout = 30;
		}

		if (Boolean.FALSE.equals(inTeLoggenMedewerker.getActief()))
		{
			resultMessage = "Uw account is gedeactiveerd. Neem contact op met uw beheerder";
			LOG.error("SE: {} {} {}", seLogCode(screeningsEenheid), accountIdLogTekst(inTeLoggenMedewerker), resultMessage);
			logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, inTeLoggenMedewerker, screeningsEenheid, proxyDatumTijd,
				"Inlogpoging met een gedeactiveerd account");
		}
		else if (authenticatieService.isAccountLocked(inTeLoggenMedewerker))
		{
			if (InlogStatus.GEBLOKKEERD.equals(inTeLoggenMedewerker.getInlogstatus()))
			{
				resultMessage = "Uw account is geblokkeerd. Neem contact op met uw beheerder";
			}
			else if (InlogStatus.TIJDELIJK_GEBLOKKEERD.equals(inTeLoggenMedewerker.getInlogstatus()))
			{
				resultMessage = "Uw account is voor " + foutieveAanmeldpogingenTimeout + " minuten geblokkeerd";
			}
			LOG.error("SE: {} {} {}", seLogCode(screeningsEenheid), accountIdLogTekst(inTeLoggenMedewerker), resultMessage);
			logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, inTeLoggenMedewerker, screeningsEenheid, proxyDatumTijd,
				"Inlogpoging met een geblokkeerd account");
		}
		else if (inTeLoggenMedewerker != null && InlogStatus.OK.equals(inTeLoggenMedewerker.getInlogstatus())
			&& inTeLoggenMedewerker.getFoutieveInlogpogingen() != null)
		{
			Integer maxFoutieveAanmeldpogingen = preferenceService.getInteger(PreferenceKey.MAXIMUM_FOUTIEVE_AANMELDPOGINGEN.name());
			if (maxFoutieveAanmeldpogingen == null)
			{
				maxFoutieveAanmeldpogingen = 3;
			}
			int aantalPogingenResterend = maxFoutieveAanmeldpogingen - inTeLoggenMedewerker.getFoutieveInlogpogingen();
			resultMessage = "Aanmelden mislukt. Aantal pogingen resterend: " + aantalPogingenResterend;
			LOG.error("SE: {} {} {}", seLogCode(screeningsEenheid), accountIdLogTekst(inTeLoggenMedewerker), resultMessage);
		}
		else
		{

			resultMessage = DEFAULT_MESSAGE;
			LOG.error("SE: {} Er is geen medewerker gevonden.", seLogCode(screeningsEenheid));
			logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, inTeLoggenMedewerker, screeningsEenheid, proxyDatumTijd,
				"Medewerker niet gevonden");
		}
		return resultMessage;
	}

	public InstellingGebruiker getIngelogdeGebruiker()
	{
		Long accountId = getLoggedInAccountId();
		return accountId != null ? hibernateService.load(InstellingGebruiker.class, accountId) : null;
	}

	public MammaScreeningsEenheid getIngelogdeScreeningsEenheid()
	{
		return hibernateService.load(MammaScreeningsEenheid.class, screeningsEenheidId);
	}

	public Long getLoggedInAccountId()
	{
		return accountId;
	}

	public static String accountIdLogTekst(Account account)
	{
		if (account instanceof InstellingGebruiker)
		{
			Integer medewerkercode = ((InstellingGebruiker) account).getMedewerker().getMedewerkercode();
			return String.format("IG:%s MC:%s", account.getId(), medewerkercode);
		}
		else if (account instanceof Gebruiker)
		{
			Integer medewerkercode = ((Gebruiker) account).getMedewerkercode();
			return String.format("G:%s MC:%s", account.getId(), medewerkercode);
		}
		return "?";
	}
}
