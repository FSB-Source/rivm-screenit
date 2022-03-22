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

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;

@Configuration
@Order(Ordered.HIGHEST_PRECEDENCE)
@ConfigurationProperties(prefix = "app")
@Setter
public class ApplicationConfig
{
	@Getter
	private final Jgroups jgroups = new Jgroups();

	private Boolean testModus;

	private String environment;

	private String instance;

	private String naam;

	private String url;

	private String filestoreLocation;

	private String planningBkRestUrl;

	private String kansberekeningServiceUrl;

	private String asposeLicense;

	private String asposeVragenlijstTemplate;

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
	public String applicatieNaam()
	{
		return naam;
	}

	@Bean
	public String applicatieInstantie()
	{
		return instance;
	}

	@Bean
	public String applicationUrl()
	{
		return url != null ? url : "";
	}

	@Bean
	public String planningBkRestUrl()
	{
		return planningBkRestUrl;
	}

	@Bean
	public String kansberekeningServiceUrl()
	{
		return kansberekeningServiceUrl;
	}

	@Bean
	public String locatieFilestore()
	{
		return filestoreLocation;
	}

	@Bean
	public Boolean testModus()
	{
		return testModus;
	}

	@Bean
	public String versionMapping()
	{
		return "";
	}

	@Bean
	public Integer jgroupsCommonBindPort()
	{
		return jgroups.common.bindPort;
	}

	@Bean
	public Integer jgroupsEhcacheBindPort()
	{
		return jgroups.ehcache.bindPort;
	}

	@Bean
	public String jgroupsCommonIPPorts()
	{
		return jgroups.common.ipPorts;
	}

	@Bean
	public String jgroupsEhcacheIPPorts()
	{
		return jgroups.ehcache.ipPorts;
	}

	@Bean
	public String jgroupsBindIP()
	{
		return jgroups.bindIp;
	}

	@Bean
	public String asposeLicence()
	{
		if (asposeLicense == null)
		{
			return filestoreLocation + "/aspose/Aspose.Words.lic";
		}
		return asposeLicense;
	}

	@Bean
	public String vragenlijstTemplate()
	{
		return asposeVragenlijstTemplate != null ? asposeVragenlijstTemplate : "";
	}

	@Bean
	public Integer planAhead()
	{
		return 0;
	}

	@Bean
	public Integer maxRoosterUitrolInMonths()
	{
		return 15;
	}

	@Setter
	private static class Jgroups
	{
		@Getter
		private final Group common = new Group();

		@Getter
		private final Group ehcache = new Group();

		private String bindIp;

		@Setter
		private static class Group
		{
			private Integer bindPort;

			private String ipPorts;

		}
	}

}
