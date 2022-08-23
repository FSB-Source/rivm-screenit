package nl.rivm.screenit.wsb.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "app")
@Setter
public class WsbApplicationConfig
{

	private String smartCardConfigDir;

	private String smartCardUziRootCer;

	private String schematronLocation;

	private Boolean fqdncontrole;

	private WsbEndpointConfig labformulier;

	private WsbEndpointConfig scan;

	private WsbEndpointConfig inpakcentrum;

	@Bean
	public String schematronLocation()
	{
		return StringUtils.defaultIfBlank(schematronLocation, "");
	}

	@Bean
	public Boolean fqdncontrole()
	{
		return fqdncontrole != null ? fqdncontrole : false;
	}

	@Bean
	public String labformulierInlognaam()
	{
		return StringUtils.defaultIfBlank(labformulier.inlognaam, "");
	}

	@Bean
	public String labformulierWachtwoord()
	{
		return StringUtils.defaultIfBlank(labformulier.wachtwoord, "");
	}

	@Bean
	public String scanInlognaam()
	{
		return StringUtils.defaultIfBlank(scan.inlognaam, "");
	}

	@Bean
	public String scanWachtwoord()
	{
		return StringUtils.defaultIfBlank(scan.wachtwoord, "");
	}

	@Bean
	public String inpakCentrumInlognaam()
	{
		return StringUtils.defaultIfBlank(inpakcentrum.inlognaam, "");
	}

	@Bean
	public String inpakCentrumWachtwoord()
	{
		return StringUtils.defaultIfBlank(inpakcentrum.wachtwoord, "");
	}

	@Bean
	public String configDir()
	{
		return org.apache.commons.lang.StringUtils.defaultIfBlank(smartCardConfigDir, "");
	}

	@Bean
	public String uziRootCertificateFile()
	{
		return org.apache.commons.lang.StringUtils.defaultIfBlank(smartCardUziRootCer, "");
	}

	@Setter
	private static class WsbEndpointConfig
	{

		private String inlognaam;

		private String wachtwoord;
	}
}
