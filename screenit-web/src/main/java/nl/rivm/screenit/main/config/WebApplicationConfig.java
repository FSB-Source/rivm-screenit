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

import org.apache.commons.lang.StringUtils;
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

	private String smartCardConfigDir;

	private String smartCardUziRootCer;

	private String antwoordFormulierUrl;

	private String handleidingenPath;

	private String huisartsPortaalUrl;

	@Bean
	public String configDir()
	{
		return StringUtils.defaultIfBlank(smartCardConfigDir, "");
	}

	@Bean
	public String uziRootCertificateFile()
	{
		return StringUtils.defaultIfBlank(smartCardUziRootCer, "");
	}

	@Bean
	public String antwoordFormulierUrl()
	{
		return StringUtils.defaultIfBlank(antwoordFormulierUrl, "");
	}

	@Bean
	public String handleidingenPath()
	{
		return StringUtils.defaultIfBlank(handleidingenPath, "");
	}

	@Bean
	public String huisartsPortaalUrl()
	{
		return StringUtils.defaultIfBlank(huisartsPortaalUrl, "");
	}
}
