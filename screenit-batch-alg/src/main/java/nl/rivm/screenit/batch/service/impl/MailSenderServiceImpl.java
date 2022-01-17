package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.MailSenderService;
import nl.rivm.screenit.model.Mail;
import nl.rivm.screenit.model.MailVerzenden;
import nl.rivm.screenit.model.enums.MailPriority;
import nl.rivm.screenit.repository.MailRepository;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.mail.MailException;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
public class MailSenderServiceImpl implements MailSenderService
{

	@Autowired
	private JavaMailSender mailSender;

	@Autowired
	@Qualifier("afzendEmailadres")
	private String afzendEmailadres;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private MailRepository mailRepository;

	@Override
	@Transactional(propagation = Propagation.REQUIRES_NEW)
	public void verzendEnVerwijderQueuedMail(Mail mail) throws MessagingException, MailException
	{
		if (sendEmail(mail.getRecipient(), mail.getSubject(), mail.getContent(), mail.getPriority()))
		{
			mailRepository.delete(mail);
		}
	}

	private boolean sendEmail(String to, String subject, String message, MailPriority priority) throws MessagingException, MailException
	{
		if (StringUtils.isNotBlank(to))
		{
			var emailAdressen = to.split("[\\s;,]");
			sendEmail(emailAdressen, subject, message, priority);
			return true;
		}
		return false;
	}

	private void sendEmail(String[] to, String subject, String message, MailPriority priority) throws MessagingException, MailException
	{
		var mailVerzenden = mailVerzenden();
		if (MailVerzenden.ALTERNATIEF_ADRES.equals(mailVerzenden))
		{
			subject += " (" + StringUtils.join(to, "; ") + ")";
			var alternatiefAdres = simplePreferenceService.getString(PreferenceKey.ALTERNATIEF_ADRES.name());
			for (int i = 0; i < to.length; i++)
			{
				to[i] = alternatiefAdres;
			}
		}

		if (!MailVerzenden.UIT.equals(mailVerzenden))
		{
			var mimeMessage = mailSender.createMimeMessage();

			var helper = createMimeMessageHelper(mimeMessage, subject, message, priority);
			helper.setTo(to);
			mailSender.send(mimeMessage);
		}
		else
		{
			LOG.warn("Mail versturen staat uit. To: " + StringUtils.join(to, ";") + " | Subject: " + subject + " | Message: " + message);
		}
	}

	private MimeMessageHelper createMimeMessageHelper(MimeMessage mimeMessage, String subject, String message, MailPriority priority) throws MessagingException
	{
		var helper = new MimeMessageHelper(mimeMessage, true);
		helper.setFrom(afzendEmailadres);
		helper.setSubject(subject);
		helper.setText(message, true);
		if (priority != null)
		{
			helper.setPriority(priority.getPriority());
		}
		return helper;
	}

	private MailVerzenden mailVerzenden()
	{
		var verzendenPreferences = simplePreferenceService.getEnum(PreferenceKey.MAIL_VERZENDEN.toString(), MailVerzenden.class);
		if (verzendenPreferences == null)
		{
			return MailVerzenden.AAN;
		}
		return verzendenPreferences;
	}
}
