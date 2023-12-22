package nl.rivm.screenit.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.nio.file.Paths;

import lombok.Setter;

import org.apache.commons.lang.StringUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;

@Configuration
@Order(Ordered.HIGHEST_PRECEDENCE)
@ConfigurationProperties(prefix = "app")
@Setter
public class ApplicationConfig
{
	private Boolean testModus;

	private String environment;

	private String instance;

	private String name;

	private String url;

	private String filestoreLocation;

	private String planningBkRestUrl;

	private String kansberekeningServiceUrl;

	private String asposeLicense;

	private String asposeVragenlijstTemplate;

	private Integer planAhead;

	private Integer maxRoosterUitrolInMonths;

	private String zooKeeperServerUri;

	private String medewerkerPortaalResourceUrl;

	private String activemqUsername;

	private String activemqPassword;

	@Bean
	public Boolean isJpa()
	{
		return true;
	}

	@Bean
	public String applicationEnvironment()
	{
		return environment;
	}

	@Bean
	public String applicationName()
	{
		return name;
	}

	@Bean
	public String applicationInstance()
	{
		return instance;
	}

	@Bean
	public String applicationUrl()
	{
		return StringUtils.defaultIfBlank(url, "");
	}

	@Bean
	public String planningBkRestUrl()
	{
		return StringUtils.defaultIfBlank(planningBkRestUrl, "");
	}

	@Bean
	public String kansberekeningServiceUrl()
	{
		return StringUtils.defaultIfBlank(kansberekeningServiceUrl, "");
	}

	@Bean
	@Profile("!test")
	public String locatieFilestore()
	{
		return Paths.get(filestoreLocation).toString();
	}

	@Bean
	public Boolean testModus()
	{
		return testModus;
	}

	@Bean
	public String asposeLicence()
	{
		return StringUtils.defaultIfBlank(asposeLicense, filestoreLocation + "/aspose/Aspose.Words.lic");
	}

	@Bean
	public String vragenlijstTemplate()
	{
		return StringUtils.defaultIfBlank(asposeVragenlijstTemplate, "");
	}

	@Bean
	public Integer planAhead()
	{
		return planAhead != null ? planAhead : 0;
	}

	@Bean
	public Integer maxRoosterUitrolInMonths()
	{
		return maxRoosterUitrolInMonths != null ? maxRoosterUitrolInMonths : 15;
	}

	@Bean
	public String zooKeeperServerUri()
	{
		return StringUtils.defaultIfBlank(zooKeeperServerUri, "");
	}

	@Bean
	public String medewerkerPortaalResourceUrl()
	{
		return StringUtils.defaultIfBlank(medewerkerPortaalResourceUrl, "");
	}

	@Bean
	public String activemqUsername()
	{
		return StringUtils.defaultIfBlank(activemqUsername, "");
	}

	@Bean
	public String activemqPassword()
	{
		return StringUtils.defaultIfBlank(activemqPassword, "");
	}

}
