package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Nonnull;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.alg.client.contact.MailAttachmentDto;
import nl.rivm.screenit.mappers.MailAttachmentMapper;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Mail;
import nl.rivm.screenit.model.MailVerzenden;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.enums.MailPriority;
import nl.rivm.screenit.model.enums.MailServerKeuze;
import nl.rivm.screenit.repository.MailRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class MailServiceImpl implements MailService
{
	private final ICurrentDateSupplier currentDateSupplier;

	private final MailRepository mailRepository;

	private final Boolean testModus;

	private final SimplePreferenceService preferenceService;

	@Autowired(required = false)
	@Qualifier("huisartsPortaalUrl")
	private String huisartsPortaalUrl;

	@Autowired(required = false)
	@Qualifier("applicationUrl")
	private String applicationUrl;

	private final MailAttachmentMapper mailAttachmentMapper;

	@Override
	public void queueMailAanProfessional(String to, String subject, String content)
	{
		var attachments = new ArrayList<MailAttachmentDto>();
		queueMail(to, subject, content, MailPriority.NORMAL, MailServerKeuze.PROFESSIONAL, attachments);
	}

	@Override
	public void queueMailAanProfessional(String to, String subject, String content, @Nonnull MailPriority priority)
	{
		var attachments = new ArrayList<MailAttachmentDto>();
		queueMail(to, subject, content, priority, MailServerKeuze.PROFESSIONAL, attachments);
	}

	@Override
	public void queueMailAanClient(Client client, String subject, String content, List<MailAttachmentDto> attachmentDtos)
	{
		if (StringUtils.isBlank(client.getPersoon().getEmailadres()))
		{
			throw new IllegalStateException(String.format("Client id: '%d' heeft geen emailadres", client.getId()));
		}
		queueMail(client.getPersoon().getEmailadres(), subject, content, MailPriority.NORMAL, MailServerKeuze.CLIENT, attachmentDtos);
	}

	private void queueMail(String to, String subject, String content, @NotNull MailPriority priority, MailServerKeuze mailServer, List<MailAttachmentDto> mailAttachmentsDto)
	{
		var attachments = mailAttachmentMapper.dtoToAttachmentList(mailAttachmentsDto);

		var mail = new Mail(currentDateSupplier.getLocalDateTime(),
			to.replaceAll("(?<=[;,])\\s+", ""),
			subject,
			content,
			priority,
			mailServer, attachments);

		attachments.forEach(mailAttachment ->
			mailAttachment.setMail(mail));

		mailRepository.save(mail);
	}

	@Override
	public MailVerzenden getMailVerzenden()
	{
		var alternatiefAdres = preferenceService.getString(PreferenceKey.ALTERNATIEF_ADRES.name());
		var mailVerzenden = preferenceService.getEnum(PreferenceKey.MAIL_VERZENDEN.name(), MailVerzenden.class);
		if (Boolean.FALSE.equals(testModus))
		{
			return MailVerzenden.AAN;
		}
		if (mailVerzenden == null || MailVerzenden.AAN.equals(mailVerzenden) || StringUtils.isBlank(alternatiefAdres))
		{
			mailVerzenden = MailVerzenden.UIT;
		}
		return mailVerzenden;
	}

	@Override
	public void sendRegistratieMail(CervixHuisarts huisarts)
	{
		try
		{
			queueProfessionalMailWithConfig(MedewerkerMailTemplateConfig.BMHK_HUISARTS_REGISTRATIE, huisarts.getOrganisatieMedewerkers().get(0).getMedewerker(),
				huisarts.getEmail());
		}
		catch (Exception e)
		{
			LOG.error("Niet mogelijk om een registratiemail uit te sturen naar huisarts({}) met agbcode {}", huisarts.getId(), huisarts.getAgbcode(), e);
		}
	}

	@Override
	public void sendPasswordResetMail(CervixHuisarts huisarts)
	{
		if (huisarts != null)
		{
			queueProfessionalMailWithConfig(MedewerkerMailTemplateConfig.BMHK_HUISARTS_WACHTWOORD_VERGETEN, huisarts.getOrganisatieMedewerkers().get(0).getMedewerker(),
				huisarts.getEmail());
		}
	}

	@Override
	public void sendWachwoordVerlooptHerinneringMail(Gebruiker gebruiker)
	{
		try
		{
			queueProfessionalMailWithConfig(MedewerkerMailTemplateConfig.WACHTWOORD_VERLOOPT_HERINNERNIG, gebruiker, gebruiker.getEmailextra());
		}
		catch (Exception e)
		{
			LOG.error("Niet mogelijk om een herinneringsmail te sturen naar gebruiker ('{}')", gebruiker.getId(), e);
		}
	}

	private void queueProfessionalMailWithConfig(MedewerkerMailTemplateConfig config, Gebruiker medewerker, String emailAdres)
	{
		if (StringUtils.isNotBlank(emailAdres))
		{
			var contentTemplate = preferenceService.getString(config.getContentTemplate().name(), config.getDefaultContentTemplate());

			var mailContent = mergeMedewerkerMail(medewerker, contentTemplate, config);

			var mailSubjectTemplate = preferenceService.getString(config.getSubjectTemplate().name(), config.getDefaultSubjectTemplate());
			queueMailAanProfessional(emailAdres, mailSubjectTemplate, mailContent);
		}
	}

	@NotNull
	private String mergeMedewerkerMail(Gebruiker medewerker, String mailContentTemplate, MedewerkerMailTemplateConfig config)
	{
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

		var code = "";
		if (StringUtils.isNotBlank(medewerker.getWachtwoordChangeCode()))
		{
			code = medewerker.getWachtwoordChangeCode();
		}

		var link = "";
		var url = config.isUseHuisartsenPortaalUrl() ? huisartsPortaalUrl : applicationUrl;
		if (StringUtils.isNotBlank(url))
		{
			if (!url.endsWith("/"))
			{
				url += "/";
			}
			link = String.format("<a href=\"%s%s/\">link</a>", url, config.getUrlContext());
		}
		mailContentTemplate = mailContentTemplate.replace("{aanhef}", aanhef);
		mailContentTemplate = mailContentTemplate.replace("{titel}", titel);
		mailContentTemplate = mailContentTemplate.replace("{achternaam}", achternaam);
		mailContentTemplate = mailContentTemplate.replace("{tussenvoegsel}", tussenvoegsel);
		mailContentTemplate = mailContentTemplate.replace("{voorletters}", voorletters);
		mailContentTemplate = mailContentTemplate.replace("{link}", link);
		mailContentTemplate = mailContentTemplate.replace("{code}", code);
		return mailContentTemplate;
	}

}
