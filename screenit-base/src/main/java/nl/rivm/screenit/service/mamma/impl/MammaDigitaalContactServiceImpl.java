package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.SmsVersturenSqsDto;
import nl.rivm.screenit.dto.alg.client.contact.MailAttachmentDto;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.SmsVerzenden;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.DigitaalBerichtTemplateType;
import nl.rivm.screenit.model.enums.DigitaalBerichtType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.SmsStatus;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDigitaalClientBericht;
import nl.rivm.screenit.repository.mamma.MammaBaseAfspraakRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.DigitaalBerichtTemplateService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.mamma.MammaDigitaalClientBerichtService;
import nl.rivm.screenit.service.mamma.MammaDigitaalContactService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.TelefoonnummerUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
@AllArgsConstructor
@Transactional(propagation = Propagation.REQUIRED)
public class MammaDigitaalContactServiceImpl implements MammaDigitaalContactService
{
	private final MailService mailService;

	private final MammaDigitaalClientBerichtService clientDigitaalBerichtService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final ClientService clientService;

	private final DigitaalBerichtTemplateService templateService;

	private final MammaBaseAfspraakRepository baseAfspraakRepository;

	private final SimplePreferenceService preferenceService;

	private final Boolean testModus;

	private final BaseBriefService baseBriefService;

	private final LogService logService;

	@Override
	public void sendBevestigAfspraakMail(MammaAfspraak afspraak)
	{
		var digitaalBerichtDTO = templateService.maakDigitaalBericht(DigitaalBerichtTemplateType.MAMMA_AFSPRAAK_BEVESTIGING, afspraak.getUitnodiging().getBrief().getClient());

		var clientMail = maakClientBericht(afspraak, DigitaalBerichtTemplateType.MAMMA_AFSPRAAK_BEVESTIGING);
		var screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
		var client = screeningRonde.getDossier().getClient();

		baseBriefService.setNietGegenereerdeBrievenOpTegenhouden(screeningRonde, Collections.singletonList(BriefType.MAMMA_AFSPRAAK_VERZET));
		stuurMail(client, digitaalBerichtDTO.getSubject(), digitaalBerichtDTO.getBody(), digitaalBerichtDTO.getAttachments());
		clientDigitaalBerichtService.saveOrUpdate(clientMail);
	}

	@Override
	public List<Long> getAfsprakenVoorSmsVersturen()
	{
		if (checkSmsVerzendenStatus())
		{
			int termijn = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_SMS_HERINNERING_TERMIJN.name());

			LocalDateTime volgendeDag = currentDateSupplier.getLocalDateTime().plusDays(1).withHour(7).withMinute(0);
			LocalDateTime termijnDatum = currentDateSupplier.getLocalDateTime().plusHours(termijn);

			return baseAfspraakRepository.findTop250AfsprakenOmSmsTeVersturen(DateUtil.toUtilDate(volgendeDag), DateUtil.toUtilDate(termijnDatum));
		}
		LOG.warn("Sms verzenden staat op uit, er wordt geen sms gestuurd.");
		return new ArrayList<>();
	}

	@Override
	public Optional<SmsVersturenSqsDto> maakSmsVersturenDTO(Long afspraakId)
	{
		if (checkSmsVerzendenStatus())
		{
			var opgehaaldeAfspraak = baseAfspraakRepository.findById(afspraakId).orElseThrow(() -> new IllegalStateException("Kan geen afspraak vinden met id: " + afspraakId));
			var optionalSmsDto = maakAfspraakSmsHerinnering(opgehaaldeAfspraak);
			if (optionalSmsDto.isEmpty())
			{
				administreerSmsGefaald(opgehaaldeAfspraak);
			}
			return optionalSmsDto;
		}
		return Optional.empty();
	}

	private SmsVerzenden getSmsVerzenden()
	{
		var alternatiefMobielNummer = preferenceService.getString(PreferenceKey.ALTERNATIEF_MOBIELNUMMER.name());
		var smsVerzenden = preferenceService.getEnum(PreferenceKey.SMS_VERZENDEN.name(), SmsVerzenden.class);
		if (Boolean.FALSE.equals(testModus))
		{
			return SmsVerzenden.AAN;
		}
		if (smsVerzenden == null || SmsVerzenden.AAN.equals(smsVerzenden) || StringUtils.isBlank(alternatiefMobielNummer))
		{
			smsVerzenden = SmsVerzenden.UIT;
		}
		return smsVerzenden;
	}

	private boolean checkSmsVerzendenStatus()
	{
		var smsVerzenden = getSmsVerzenden();
		return smsVerzenden == SmsVerzenden.ALTERNATIEF_MOBIELNUMMER || smsVerzenden == SmsVerzenden.AAN;
	}

	private Optional<SmsVersturenSqsDto> maakAfspraakSmsHerinnering(MammaAfspraak afspraak)
	{
		var client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
		var smsBerichtTekst = templateService.maakDigitaalBericht(DigitaalBerichtTemplateType.MAMMA_AFSPRAAK_HERINNERING, client).getBody();
		return maakSmsVersturenSqsDto(client, smsBerichtTekst);
	}

	@NotNull
	private Optional<SmsVersturenSqsDto> maakSmsVersturenSqsDto(Client client, String smsBericht)
	{
		var telefoonNummer = client.getPersoon().getTelefoonnummer1();
		if (TelefoonnummerUtil.isCorrectNederlandsMobielNummer(telefoonNummer))
		{
			var sms = new SmsVersturenSqsDto();
			if (getSmsVerzenden() == SmsVerzenden.ALTERNATIEF_MOBIELNUMMER)
			{
				sms.setTekst(smsBericht + " [SMS verstuurd naar " + telefoonNummer + "]");
				sms.setTelefoonnummer(standaardiseerNederlandsMobielNummer(preferenceService.getString(PreferenceKey.ALTERNATIEF_MOBIELNUMMER.toString())));
			}
			else
			{
				sms.setTekst(smsBericht);
				sms.setTelefoonnummer(standaardiseerNederlandsMobielNummer(telefoonNummer));
			}
			return Optional.of(sms);
		}
		LOG.error("Geen correct mobielnummer van clientId " + client.getId());
		return Optional.empty();
	}

	private static String standaardiseerNederlandsMobielNummer(String mobielNummer)
	{
		var blankOfHyphenRegex = "[- ]";
		if (mobielNummer.matches("^(06[- ]?\\d{8})$"))
		{
			return mobielNummer.replaceAll(blankOfHyphenRegex, "").replaceFirst("0", "31");
		}
		if (mobielNummer.matches("^(00316[- ]?\\d{8})$"))
		{
			return mobielNummer.replaceAll(blankOfHyphenRegex, "").replaceFirst("00", "");

		}
		return mobielNummer.replaceAll(blankOfHyphenRegex, "").replaceFirst("[+]", "");
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW)
	public void administreerSmsVerstuurd(List<Long> afspraakIds)
	{
		for (Long afspraak : afspraakIds)
		{
			var opgehaaldeAfspraak = baseAfspraakRepository.findById(afspraak).orElseThrow(() -> new IllegalStateException("Kan geen afspraak vinden met id: " + afspraak));

			opgehaaldeAfspraak.setSmsStatus(SmsStatus.VERSTUURD);
			baseAfspraakRepository.save(opgehaaldeAfspraak);
			clientDigitaalBerichtService.saveOrUpdate(maakClientBericht(opgehaaldeAfspraak, DigitaalBerichtTemplateType.MAMMA_AFSPRAAK_HERINNERING));
		}
	}

	private void administreerSmsGefaald(MammaAfspraak afspraak)
	{
		LOG.info("Er is iets fout gegaan met aanmaken van sms DTO van afspraak (id: '{}')", afspraak.getId());
		var persoon = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
		afspraak.setSmsStatus(SmsStatus.GEFAALD);
		clientDigitaalBerichtService.saveOrUpdate(maakClientBerichtSmsGefaald(afspraak, DigitaalBerichtTemplateType.MAMMA_AFSPRAAK_HERINNERING, persoon));
		baseAfspraakRepository.save(afspraak);
	}

	private MammaDigitaalClientBericht maakClientBerichtSmsGefaald(MammaAfspraak afspraak, DigitaalBerichtTemplateType digitaalBerichtTemplateType, Client client)
	{
		var persoon = client.getPersoon();
		var clientBericht = maakClientBericht(afspraak, digitaalBerichtTemplateType);
		clientBericht.setVerzendenGefaald(true);
		var meldingLogGebeurtenis = "";

		if (StringUtils.isBlank(persoon.getTelefoonnummer1()))
		{
			meldingLogGebeurtenis = "Geen mobielnummer gevonden";
			clientBericht.setOntvanger("Geen");
		}
		else if (!TelefoonnummerUtil.isCorrectNederlandsMobielNummer(persoon.getTelefoonnummer1()))
		{
			meldingLogGebeurtenis = "Geen geldig nummer: " + persoon.getTelefoonnummer1();
		}
		else
		{
			meldingLogGebeurtenis = "Onbekende reden";
		}

		clientBericht.setOmschrijving(meldingLogGebeurtenis);
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_DIGITAAL_CONTACT_SMS_VERSTUREN_GEFAALD, client, meldingLogGebeurtenis,
			Bevolkingsonderzoek.MAMMA);

		return clientBericht;
	}

	@Override
	public void herzendBevestigAfspraakMail(MammaDigitaalClientBericht clientMail, String ontvangendeEmailAdres, Account ingelogdeGebruiker)
	{
		var afspraak = clientMail.getScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak();
		var emailBericht = templateService.maakDigitaalBericht(DigitaalBerichtTemplateType.MAMMA_AFSPRAAK_BEVESTIGING, afspraak.getUitnodiging().getBrief().getClient());

		var nieuweClientMail = maakKopieClientBericht(clientMail, ontvangendeEmailAdres);
		var client = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient();
		client.getPersoon().setEmailadres(ontvangendeEmailAdres);

		stuurMail(client, emailBericht.getSubject(), emailBericht.getBody(), emailBericht.getAttachments());
		clientDigitaalBerichtService.saveOrUpdate(nieuweClientMail);
		clientService.saveContactGegevens(client, ingelogdeGebruiker);
	}

	private void stuurMail(Client client, String mailSubject, String mailContent, List<MailAttachmentDto> mailAttachments)
	{
		mailService.queueMailAanClient(client, mailSubject, mailContent, mailAttachments);
	}

	private MammaDigitaalClientBericht maakClientBericht(MammaAfspraak afspraak, DigitaalBerichtTemplateType digitaalBerichtTemplateType)
	{
		var clientBericht = new MammaDigitaalClientBericht();
		clientBericht.setCreatieMoment(currentDateSupplier.getLocalDateTime());
		setClientBerichtOntvanger(afspraak, digitaalBerichtTemplateType.getBerichtType(), clientBericht);
		clientBericht.setDigitaalBerichtTemplateType(digitaalBerichtTemplateType);
		clientBericht.setScreeningRonde(afspraak.getUitnodiging().getScreeningRonde());
		clientBericht.setVerzendenGefaald(false);

		return clientBericht;
	}

	private static void setClientBerichtOntvanger(MammaAfspraak afspraak, DigitaalBerichtType digitaalBerichtType, MammaDigitaalClientBericht clientBericht)
	{
		var persoon = afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient().getPersoon();
		if (digitaalBerichtType == DigitaalBerichtType.SMS)
		{
			clientBericht.setOntvanger(persoon.getTelefoonnummer1());
		}
		else
		{
			clientBericht.setOntvanger(persoon.getEmailadres());
		}
	}

	private MammaDigitaalClientBericht maakKopieClientBericht(MammaDigitaalClientBericht origineelBericht, String ontvanger)
	{
		var kopieClientBericht = new MammaDigitaalClientBericht();
		kopieClientBericht.setCreatieMoment(currentDateSupplier.getLocalDateTime());
		kopieClientBericht.setOntvanger(ontvanger);
		kopieClientBericht.setDigitaalBerichtTemplateType(origineelBericht.getDigitaalBerichtTemplateType());
		kopieClientBericht.setScreeningRonde(origineelBericht.getScreeningRonde());
		kopieClientBericht.setIsHerzonden(true);
		kopieClientBericht.setVerzendenGefaald(false);

		return kopieClientBericht;
	}
}
