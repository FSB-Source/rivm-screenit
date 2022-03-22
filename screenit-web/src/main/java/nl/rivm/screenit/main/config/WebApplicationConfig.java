package nl.rivm.screenit.main.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import lombok.Setter;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;

@Configuration
@Order(Ordered.HIGHEST_PRECEDENCE)
@ConfigurationProperties(prefix = "app")
@Setter
public class WebApplicationConfig
{

	private String configDir;

	private String uziRootCer;

	private String antwoordFormulierUrl;

	private String voFileStorePath;

	private String handleidingenPath;

	private String clientportaalUrl;

	private String clientportaalUrlAutoLogin;

	private String clientportaalUrlAutoLoginSecret;

	private String spherionUrl;

	private String spherionUsername;

	private String spherionPassword;

	private String huisartsPortaalUrl;

	private String zooKeeperServerUri;

	private String hpvHost;

	private Integer hpvPort;

	private String hl7IfobtHost;

	private Integer hl7IfobtPort;

	private Integer hl7ImsPort;

	private Integer hl7IlmPort;

	@Bean
	public String configDir()
	{
		return configDir != null ? configDir : "";
	}

	@Bean
	public String uziRootCertificateFile()
	{
		return uziRootCer != null ? uziRootCer : "";
	}

	@Bean
	public String antwoordFormulierUrl()
	{
		return antwoordFormulierUrl != null ? antwoordFormulierUrl : "";
	}

	@Bean
	public String voFileStorePath()
	{
		return voFileStorePath != null ? voFileStorePath : "";
	}

	@Bean
	public String handleidingenPath()
	{
		return handleidingenPath != null ? handleidingenPath : "";
	}

	@Bean
	public String clientportaalUrl()
	{
		return clientportaalUrl != null ? clientportaalUrl : "";
	}

	@Bean
	public String clientportaalUrlAutoLogin()
	{
		return clientportaalUrlAutoLogin != null ? clientportaalUrlAutoLogin : "";
	}

	@Bean
	public String clientportaalUrlAutoLoginSecret()
	{
		return clientportaalUrlAutoLoginSecret != null ? clientportaalUrlAutoLoginSecret : "";
	}

	@Bean
	public String spherionUrl()
	{
		return spherionUrl != null ? spherionUrl : "";
	}

	@Bean
	public String spherionUsername()
	{
		return spherionUsername != null ? spherionUsername : "";
	}

	@Bean
	public String spherionPassword()
	{
		return spherionPassword != null ? spherionPassword : "";
	}

	@Bean
	public String huisartsPortaalUrl()
	{
		return huisartsPortaalUrl != null ? huisartsPortaalUrl : "";
	}

	@Bean
	public String zooKeeperServerUri()
	{
		return zooKeeperServerUri != null ? zooKeeperServerUri : "";
	}

	@Bean
	public String hpvHost()
	{
		return hpvHost != null ? hpvHost : "";
	}

	@Bean
	public Integer hpvPort()
	{
		return hpvPort != null ? hpvPort : 0;
	}

	@Bean
	public String hl7IfobtHost()
	{
		return hl7IfobtHost != null ? hl7IfobtHost : "";
	}

	@Bean
	public Integer hl7IfobtPort()
	{
		return hl7IfobtPort != null ? hl7IfobtPort : 0;
	}

	@Bean
	public Integer hl7ImsPort()
	{
		return hl7ImsPort != null ? hl7ImsPort : 0;
	}

	@Bean
	public Integer hl7IlmPort()
	{
		return hl7IlmPort != null ? hl7IlmPort : 0;
	}
}
