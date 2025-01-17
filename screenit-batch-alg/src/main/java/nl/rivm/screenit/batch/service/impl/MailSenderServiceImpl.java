package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.MailSenderService;
import nl.rivm.screenit.model.Mail;
import nl.rivm.screenit.model.MailAttachment;
import nl.rivm.screenit.model.MailVerzenden;
import nl.rivm.screenit.model.enums.MailPriority;
import nl.rivm.screenit.model.enums.MailServerKeuze;
import nl.rivm.screenit.repository.algemeen.MailRepository;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.util.mail.LoggingJavaMailSender;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.mail.MailException;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
@AllArgsConstructor
public class MailSenderServiceImpl implements MailSenderService
{
	private final LoggingJavaMailSender mailSenderProfessional;

	private final LoggingJavaMailSender mailSenderClient;

	private final SimplePreferenceService simplePreferenceService;

	private final MailRepository mailRepository;

	private final MailService mailService;

	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW)
	public void verzendEnVerwijderQueuedMail(Mail mail) throws MessagingException, MailException
	{
		if (sendEmail(mail.getRecipient(), mail.getSubject(), mail.getContent(), mail.getPriority(), mail.getMailserver(), mail.getAttachments()))
		{
			mailRepository.delete(mail);
		}
	}

	private boolean sendEmail(String to, String subject, String message, MailPriority priority, MailServerKeuze mailserver, List<MailAttachment> mailAttachment)
		throws MessagingException, MailException
	{
		if (StringUtils.isNotBlank(to))
		{
			var emailAdressen = to.split("[\\s;,]");
			sendEmail(emailAdressen, subject, message, priority, mailserver, mailAttachment);
			return true;
		}
		return false;
	}

	private void sendEmail(String[] to, String subject, String message, MailPriority priority, MailServerKeuze mailserver, List<MailAttachment> mailAttachment)
		throws MessagingException, MailException
	{
		var mailVerzenden = mailService.getMailVerzenden();
		if (MailVerzenden.ALTERNATIEF_ADRES.equals(mailVerzenden))
		{
			subject += " (" + StringUtils.join(to, "; ") + ")";
			var alternatiefAdres = simplePreferenceService.getString(PreferenceKey.ALTERNATIEF_ADRES.name());
			if (StringUtils.isNotBlank(alternatiefAdres))
			{
				Arrays.fill(to, alternatiefAdres);
			}
		}

		if (!MailVerzenden.UIT.equals(mailVerzenden))
		{
			var mailSender = getMailSender(mailserver);
			var mimeMessage = mailSender.createMimeMessage();

			var helper = createMimeMessageHelper(mimeMessage, subject, message, priority, mailSender.getFromAddress(), mailAttachment);
			helper.setTo(to);
			mailSender.send(mimeMessage);
		}
		else
		{
			LOG.warn("Mail versturen staat uit.");
		}
	}

	private LoggingJavaMailSender getMailSender(MailServerKeuze mailserver)
	{
		return mailserver == MailServerKeuze.CLIENT ? mailSenderClient : mailSenderProfessional;
	}

	private MimeMessageHelper createMimeMessageHelper(MimeMessage mimeMessage, String subject, String message, MailPriority priority, String fromAddress,
		List<MailAttachment> mailAttachmentList) throws MessagingException
	{
		var helper = new MimeMessageHelper(mimeMessage, true, StandardCharsets.UTF_8.name());
		helper.setFrom(fromAddress);
		helper.setSubject(subject);
		helper.setText(message, true);

		voegAttachmentToeAanEmail(mailAttachmentList, helper);

		if (priority != null)
		{
			helper.setPriority(priority.getPriority());
		}

		return helper;
	}

	private void voegAttachmentToeAanEmail(List<MailAttachment> mailAttachmentList, MimeMessageHelper helper) throws MessagingException
	{
		if (mailAttachmentList != null)
		{
			for (var mailAttachment : mailAttachmentList)
			{
				var decodedContent = getDecodedContent(mailAttachment.getContent());

				if (mailAttachment.getInlineContentId() != null)
				{
					helper.getMimeMessage().setHeader("CONTENT-ID", mailAttachment.getInlineContentId());
					helper.addInline(mailAttachment.getInlineContentId(), new ByteArrayResource(decodedContent), mailAttachment.getContentType());

					helper.getMimeMultipart().getBodyPart("<" + mailAttachment.getInlineContentId() + ">").setFileName(mailAttachment.getFileName());
				}
				else
				{
					helper.addAttachment(mailAttachment.getFileName(), new ByteArrayResource(decodedContent), mailAttachment.getContentType());
				}
			}
		}
	}

	private static byte[] getDecodedContent(String content)
	{
		return Base64.getDecoder().decode(content);
	}
}
