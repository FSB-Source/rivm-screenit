package nl.rivm.screenit.batch.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.List;

import lombok.Setter;

import nl.rivm.screenit.batch.model.AffinityDomain;

import org.apache.commons.lang.StringUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "xds")
@Setter
public class XdsConfig
{

	private String codeTablesFilenames;

	private String bppcPoliciesFile;

	private String configurationLocation;

	private AffinityDomainConfig affinityDomain;

	@Setter
	private static class AffinityDomainConfig
	{

		private AffinityDomain bppc;

		private AffinityDomain pdf;

	}

	@Bean
	public String bppcPoliciesFile()
	{
		return StringUtils.defaultIfBlank(bppcPoliciesFile, "");
	}

	@Bean
	public String configurationLocation()
	{
		return StringUtils.defaultIfBlank(configurationLocation, "");
	}

	@Bean
	public AffinityDomain affinityDomainBPPC()
	{
		return affinityDomain.bppc;
	}

	@Bean
	public AffinityDomain affinityDomainPdf()
	{
		return affinityDomain.pdf;
	}

	@Bean
	public List<String> codetables()
	{
		return List.of("/bppc/screenit_AD.xml");
	}

	@Bean
	public List<String> codeTablesFilenames()
	{
		return List.of(StringUtils.defaultIfBlank(codeTablesFilenames, "classpath:bppc/screenit_affinity_domain.xml"));
	}

}
