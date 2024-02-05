package nl.rivm.screenit.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
@ConfigurationProperties(prefix = "s3")
@Setter
public class S3BucketConfig
{

	private Boolean enabled;

	private String endpointOverride;

	private String accessId;

	private String accessSecret;

	private String region;

	private String name;

	@Bean
	public Boolean s3bucketEnabled()
	{
		return enabled != null && enabled;
	}

	@Bean
	public String s3bucketEndpointOverride()
	{
		return StringUtils.defaultIfBlank(endpointOverride, "");
	}

	@Bean
	public String s3bucketAccessId()
	{
		return StringUtils.defaultIfBlank(accessId, "");
	}

	@Bean
	public String s3bucketAccessSecret()
	{
		return StringUtils.defaultIfBlank(accessSecret, "");
	}

	@Bean
	public String s3bucketRegion()
	{
		return StringUtils.defaultIfBlank(region, "");
	}

	@Bean
	public String s3bucketName()
	{
		return StringUtils.defaultIfBlank(name, "");
	}

}
