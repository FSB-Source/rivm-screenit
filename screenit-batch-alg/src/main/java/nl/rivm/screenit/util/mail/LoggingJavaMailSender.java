package nl.rivm.screenit.util.mail;

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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;

import nl.rivm.screenit.service.TechnischeBerichtenLoggingSaverService;

import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessagePreparator;

public class LoggingJavaMailSender implements JavaMailSender
{

	private JavaMailSender mailSender;

	private TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService;

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

	public void setMailSender(JavaMailSender mailSender)
	{
		this.mailSender = mailSender;
	}

	public JavaMailSender getMailSender()
	{
		return this.mailSender;
	}

	public TechnischeBerichtenLoggingSaverService getTechnischeLoggingSaverService()
	{
		return technischeBerichtenLoggingSaverService;
	}

	public void setTechnischeLoggingSaverService(TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService)
	{
		this.technischeBerichtenLoggingSaverService = technischeBerichtenLoggingSaverService;
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
