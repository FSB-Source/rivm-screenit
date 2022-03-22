package nl.rivm.screenit.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.edi.service.impl.EdiMessageServiceImpl;
import nl.rivm.screenit.edi.service.impl.ValidatedMessageFactoryImpl;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.loginformatie.model.Gebeurtenis;
import nl.topicuszorg.loginformatie.services.ILogInformatieService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "edi")
@Setter
public class EdiConfig
{

	@Autowired
	private LogService logInformatieService;

	private String ediAfleverAdres;

	private String fromAddress;

	private String applicationId;

	@Getter
	private Mail mail;

	@Getter
	private SMTP smtp;

	@Setter
	private static class Mail
	{
		@Getter
		private Relay relay;

		@Setter
		private static class Relay
		{
			private String ip;

			private Integer port;
		}
	}

	@Setter
	private static class SMTP
	{
		private boolean overSsl;

		@Getter
		private Auth auth;

		@Setter
		private static class Auth
		{
			private String password;

			private String username;
		}
	}

	@Bean
	public EdiMessageServiceImpl ediMessageService()
	{
		var ediMessageService = new EdiMessageServiceImpl();
		ediMessageService.setLogInformatieService((ILogInformatieService<?, Void, Gebeurtenis>) logInformatieService);
		ediMessageService.setMailRelayIP(mailRelayIp());
		ediMessageService.setMailRelayPort(mailRelayPort());
		ediMessageService.setSmtpPort(smtpPort());
		ediMessageService.setSmtpOverSsl(smtpOverSsl());
		ediMessageService.setSmtpAuthUsername(smtpAuthUsername());
		ediMessageService.setSmtpAuthPassword(smtpAuthPassword());
		ediMessageService.setFromAddress(fromAddress());
		return ediMessageService;
	}

	@Bean
	public ValidatedMessageFactoryImpl validatedMessageFactory()
	{
		final ValidatedMessageFactoryImpl validatedMessageFactory = new ValidatedMessageFactoryImpl();
		validatedMessageFactory.setEdifactHandlingProperties("nl/rivm/screenit/edi/xml/edimessages.properties");
		validatedMessageFactory.setApplicationId(applicationId());
		validatedMessageFactory.init();
		return validatedMessageFactory;
	}

	@Bean
	public String jndiSmtpBindIP()
	{
		return "";
	}

	@Bean
	public String smtpIP()
	{
		return "";
	}

	@Bean
	public Integer smtpPort()
	{
		return 0;
	}

	@Bean
	public String mailRelayIp()
	{
		return mail != null ? mail.relay.ip : "";
	}

	@Bean
	public Integer mailRelayPort()
	{
		return mail != null ? mail.relay.port : 0;
	}

	@Bean
	public Boolean smtpOverSsl()
	{
		return smtp != null && smtp.overSsl;
	}

	@Bean
	public String smtpAuthUsername()
	{
		return smtp != null ? smtp.auth.username : "";
	}

	@Bean
	public String smtpAuthPassword()
	{
		return smtp != null ? smtp.auth.password : "";
	}

	@Bean
	public String fromAddress()
	{
		return fromAddress != null ? fromAddress : "";
	}

	@Bean
	public String ediAfleverAdres()
	{
		return ediAfleverAdres != null ? ediAfleverAdres : "devnull@topicus.nl";
	}

	@Bean
	public String applicationId()
	{
		return applicationId != null ? applicationId : "";
	}
}
