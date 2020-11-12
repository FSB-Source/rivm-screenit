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

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.MailVerzenden;
import nl.rivm.screenit.service.AsyncMailer;
import nl.rivm.screenit.service.MailService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.mail.MailException;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;

@Service
public class MailServiceImpl implements MailService
{
	private static final Logger LOG = LoggerFactory.getLogger(MailServiceImpl.class);

	@Autowired
	private JavaMailSender mailSender;

	@Autowired
	@Qualifier("afzendEmailadres")
	private String afzendEmailadres;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Override
	public boolean sendEmail(String to, String subject, String message)
	{
		return sendEmail(to, subject, message, null);
	}

	@Override
	public boolean sendEmail(String[] to, String subject, String message)
	{
		return sendEmail(to, subject, message, null);
	}

	@Override
	public boolean sendEmail(String to, String subject, String message, MailPriority priority)
	{
		if (StringUtils.isNotBlank(to))
		{
			String[] emailAdressen = to.split("[\\s;,]");
			return sendEmail(emailAdressen, subject, message, priority);
		}
		return false;
	}

	@Override
	public boolean sendEmail(String[] to, String subject, String message, MailPriority priority)
	{
		MailVerzenden mailVerzenden = mailVerzenden();
		if (MailVerzenden.ALTERNATIEF_ADRES.equals(mailVerzenden))
		{
			subject += " (" + StringUtils.join(to, "; ") + ")";
			String alternatiefAdres = simplePreferenceService.getString(PreferenceKey.ALTERNATIEF_ADRES.name());
			for (int i = 0; i < to.length; i++)
			{
				to[i] = alternatiefAdres;
			}
		}

		if (!MailVerzenden.UIT.equals(mailVerzenden))
		{
			MimeMessage mimeMessage = mailSender.createMimeMessage();
			try
			{
				MimeMessageHelper helper = createMimeMessageHelper(mimeMessage, subject, message, priority);
				helper.setTo(to);
				mailSender.send(mimeMessage);
				return true;
			}
			catch (MessagingException me)
			{
				LOG.error("Error creating mail : " + me.getMessage(), me);
			}
			catch (MailException me)
			{
				LOG.error("Error sending mail : " + me.getMessage(), me);
			}
			return false;
		}
		return true;
	}

	private static final ExecutorService EXECUTOR_SERVICE = Executors.newSingleThreadExecutor();

	@Override
	public void sendAsyncMail(AsyncMailer asyncMailer)
	{
		EXECUTOR_SERVICE.submit(asyncMailer);
	}

	private MimeMessageHelper createMimeMessageHelper(MimeMessage mimeMessage, String subject, String message, MailPriority priority) throws MessagingException
	{
		MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true);
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
		MailVerzenden verzendenPreferences = simplePreferenceService.getEnum(PreferenceKey.MAIL_VERZENDEN.toString(), MailVerzenden.class);
		if (verzendenPreferences == null)
		{
			return MailVerzenden.AAN;
		}
		return verzendenPreferences;
	}
}
