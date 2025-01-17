package nl.rivm.screenit.util.mail;

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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Properties;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;

import nl.rivm.screenit.batch.config.MailServerConfig;
import nl.rivm.screenit.service.TechnischeBerichtenLoggingSaverService;

import org.apache.commons.lang.StringUtils;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessagePreparator;

public class LoggingJavaMailSender implements JavaMailSender
{
	private static final String SMTP_TIMEOUT_MS = "30000";

	private JavaMailSenderImpl mailSender;

	private final MailServerConfig mailServerConfig;

	private final TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService;

	public LoggingJavaMailSender(MailServerConfig mailServerConfig, TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService)
	{
		this.mailServerConfig = mailServerConfig;
		this.technischeBerichtenLoggingSaverService = technischeBerichtenLoggingSaverService;
		createAndConfigureMailSender();
	}

	private void createAndConfigureMailSender()
	{
		mailSender = new JavaMailSenderImpl();
		mailSender.setUsername(mailServerConfig.getUsername());
		mailSender.setPassword(mailServerConfig.getPassword());
		mailSender.setHost(mailServerConfig.getHost());
		mailSender.setPort(mailServerConfig.getPort());
		mailSender.setProtocol("smtp");

		configureSmtpProperties(mailSender.getJavaMailProperties());
	}

	private void configureSmtpProperties(Properties mailProperties)
	{
		mailProperties.setProperty("mail.smtp.connectiontimeout", SMTP_TIMEOUT_MS);
		mailProperties.setProperty("mail.smtp.timeout", SMTP_TIMEOUT_MS);
		mailProperties.setProperty("mail.smtp.auth", StringUtils.isBlank(mailServerConfig.getUsername()) ? "false" : "true");

		if (mailServerConfig.isSsl())
		{
			mailProperties.setProperty("mail.smtp.starttls.enable", "true");
			mailProperties.setProperty("mail.smtp.ssl.protocols", "TLSv1.2");
		}
		else
		{
			mailProperties.setProperty("mail.smtp.starttls.enable", "false");
		}
	}

	public String getFromAddress()
	{
		return mailServerConfig.getFromAddress();
	}

	@Override
	public void send(SimpleMailMessage simpleMessage)
	{
		mailSender.send(simpleMessage);
		logMail(simpleMessage.getText());
	}

	@Override
	public void send(SimpleMailMessage... simpleMessages)
	{
		Arrays.stream(simpleMessages).forEach(this::send);
	}

	@Override
	public MimeMessage createMimeMessage()
	{
		return mailSender.createMimeMessage();
	}

	@Override
	public MimeMessage createMimeMessage(InputStream contentStream)
	{
		return mailSender.createMimeMessage(contentStream);
	}

	@Override
	public void send(MimeMessage mimeMessage)
	{
		mailSender.send(mimeMessage);
		logMessage(mimeMessage);
	}

	@Override
	public void send(MimeMessage... mimeMessages)
	{
		Arrays.stream(mimeMessages).forEach(this::send);
	}

	@Override
	public void send(MimeMessagePreparator mimeMessagePreparator)
	{
		mailSender.send(mimeMessagePreparator);
	}

	@Override
	public void send(MimeMessagePreparator... mimeMessagePreparators)
	{
		mailSender.send(mimeMessagePreparators);
	}

	private void logMessage(MimeMessage mimeMessage)
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();

		String message;
		try
		{
			mimeMessage.writeTo(output);
			String encoding = mimeMessage.getEncoding();

			if (encoding != null)
			{
				message = output.toString(encoding);
			}
			else
			{
				message = output.toString();
			}
		}
		catch (IOException | MessagingException e)
		{
			message = e.getMessage();
		}
		logMail(message);
	}

	private void logMail(String message)
	{
		technischeBerichtenLoggingSaverService.logRequest("MAIL_OUT", "MAIL", message);
	}

}
