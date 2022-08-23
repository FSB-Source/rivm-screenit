package nl.rivm.screenit.batch.config;

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

import java.util.Properties;

import lombok.Setter;

import nl.rivm.screenit.service.TechnischeBerichtenLoggingSaverService;
import nl.rivm.screenit.util.mail.LoggingJavaMailSender;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.mail.javamail.JavaMailSenderImpl;

@Configuration
@ConfigurationProperties(prefix = "mail")
@Setter
public class MailConfig
{
	private final TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService;

	private static final String SMTP_TIMEOUT_MS = "30000";

	private String username;

	private String password;

	private String host;

	private int port;

	private boolean ssl;

	@Autowired
	public MailConfig(TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService)
	{
		this.technischeBerichtenLoggingSaverService = technischeBerichtenLoggingSaverService;
	}

	@Bean
	public LoggingJavaMailSender mailSender()
	{
		final LoggingJavaMailSender loggingJavaMailSender = new LoggingJavaMailSender();
		final JavaMailSenderImpl mailSender = new JavaMailSenderImpl();

		mailSender.setUsername(username);
		mailSender.setPassword(password);
		mailSender.setHost(host);
		mailSender.setPort(port);
		mailSender.setProtocol("smtp");

		Properties props = mailSender.getJavaMailProperties();
		if (username == null)
		{
			props.setProperty("mail.smtp.auth", "false");
		}
		else
		{
			props.setProperty("mail.smtp.auth", "true");
		}

		if (ssl)
		{
			props.setProperty("mail.smtp.starttls.enable", "true");
			props.setProperty("mail.smtp.ssl.protocols", "TLSv1.2");
		}
		else
		{
			props.setProperty("mail.smtp.starttls.enable", "false");
		}
		props.setProperty("mail.smtp.connectiontimeout", SMTP_TIMEOUT_MS);
		props.setProperty("mail.smtp.timeout", SMTP_TIMEOUT_MS);

		loggingJavaMailSender.setMailSender(mailSender);
		loggingJavaMailSender.setTechnischeLoggingSaverService(technischeBerichtenLoggingSaverService);
		return loggingJavaMailSender;
	}
}
