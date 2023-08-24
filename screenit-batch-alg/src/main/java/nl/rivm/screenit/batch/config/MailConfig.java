package nl.rivm.screenit.batch.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.Setter;

import nl.rivm.screenit.service.TechnischeBerichtenLoggingSaverService;
import nl.rivm.screenit.util.mail.LoggingJavaMailSender;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "mail")
@Setter
public class MailConfig
{
	private final TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService;

	private MailServerConfig professional;

	private MailServerConfig client;

	@Autowired
	public MailConfig(TechnischeBerichtenLoggingSaverService technischeBerichtenLoggingSaverService)
	{
		this.technischeBerichtenLoggingSaverService = technischeBerichtenLoggingSaverService;
	}

	@Bean
	public LoggingJavaMailSender mailSenderProfessional()
	{
		return new LoggingJavaMailSender(professional, technischeBerichtenLoggingSaverService);
	}

	@Bean
	public LoggingJavaMailSender mailSenderClient()
	{
		return new LoggingJavaMailSender(client, technischeBerichtenLoggingSaverService);
	}

}
