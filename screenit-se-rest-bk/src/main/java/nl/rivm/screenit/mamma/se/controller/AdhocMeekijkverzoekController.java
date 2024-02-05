package nl.rivm.screenit.mamma.se.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import javax.servlet.http.HttpServletRequest;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.mamma.se.security.SEAccountResolverDelegate;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.SELogService;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MailPriority;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping("/api/adhocMeekijkverzoek")
public class AdhocMeekijkverzoekController extends AuthorizedController
{

	private static final String EMAIL_ONDERWERP = "AD HOC meekijk verzoek BVO BK";

	@Autowired
	private MailService mailService;

	@Autowired
	private MammaBaseFactory baseFactory;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private SELogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MammaAfspraakService afspraakService;

	@RequestMapping(value = "/indienen/{afspraakId}", method = RequestMethod.POST)
	public ResponseEntity adhocIndienen(@RequestBody String reden, @PathVariable long afspraakId, HttpServletRequest request)
	{
		LOG.info("Meekijkverzoek LRCB aangevraagd voor afspraakId: " + afspraakId + " met reden: " + reden);
		if (!isAuthorized(request, Recht.GEBRUIKER_SCREENING_MAMMA_SE_INSCHRIJVEN))
		{
			return createUnauthorizedResponse();
		}
		var account = getInstellingGebruiker(request);
		SEAccountResolverDelegate.setInstellingGebruiker(account);
		var se = getScreeningsEenheid(request);

		var afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(afspraakId, account);
		if (afspraak.getOnderzoek().getMeekijkverzoek() != null)
		{
			LOG.warn(String.format("Bestaat al een meekijkverzoek LRCB voor onderzoekId: %s met reden: %s", afspraak.getOnderzoek().getId(), reden));
			return ResponseEntity.status(HttpStatus.PRECONDITION_FAILED).build();
		}
		else if (afspraak.getOnderzoek().isDoorgevoerd())
		{
			LOG.warn(String.format("Onderzoek met onderzoekId: %s is doorgevoerd en hiervoor kan geen meekijkverzoek worden ingediend.", afspraak.getOnderzoek().getId()));
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}
		var adHocVolgNr = baseFactory.maakAdhocMeekijkverzoek(afspraak.getOnderzoek(), reden).getVolgnummer();
		var logevent = String.format("Volgnummer: %s, reden: %s", adHocVolgNr, reden);
		logService.logInfo(LogGebeurtenis.MAMMA_SE_LRCB_MEEKIJKVERZOEK, account, afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient(), se,
			currentDateSupplier.getLocalDateTime(), logevent);
		var lrcb_emailadres = preferenceService.getString(PreferenceKey.MAMMA_MEEKIJKVERZOEK_MAIL_ADRES.name());

		var emailBody = String.format("Beste LRCB,<br />" +
				"<br />" +
				"Er staat een nieuw meekijk verzoek gereed op jullie AD HOC meekijk werklijst in ScreenIT.<br />" +
				"Het betreft onderzoek met volgnummer: %s en reden: %s.<br />" +
				"Dit is een onderzoek van %s.<br />" +
				"<br />" +
				"Login in ScreenIT om beelden te bekijken.<br />" +
				"<br />" +
				"Dit is een automatisch gegenereerde email, beantwoorden is niet mogelijk.", adHocVolgNr, reden,
			afspraak.getStandplaatsPeriode().getScreeningsEenheid().getCode());
		mailService.queueMailAanProfessional(lrcb_emailadres, EMAIL_ONDERWERP, emailBody, MailPriority.HIGH);

		return ResponseEntity.ok().build();
	}

	@RequestMapping(value = "/controleren/{afspraakId}", method = RequestMethod.POST)
	public ResponseEntity adhocControle(@PathVariable long afspraakId, HttpServletRequest request)
	{
		var afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(afspraakId, getInstellingGebruiker(request));
		if (afspraak.getOnderzoek().getMeekijkverzoek() != null)
		{
			return ResponseEntity.status(HttpStatus.PRECONDITION_FAILED).build();
		}
		else if (afspraak.getOnderzoek().isDoorgevoerd())
		{
			return ResponseEntity.status(HttpStatus.CONFLICT).build();
		}
		else
		{
			return ResponseEntity.ok().build();
		}
	}
}
