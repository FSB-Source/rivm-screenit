package nl.rivm.screenit.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.net.URI;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang.StringUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.sqs.SqsClient;
import software.amazon.awssdk.services.sqs.SqsClientBuilder;

@Slf4j
@Configuration
@ConfigurationProperties(prefix = "sms")
@Setter
public class SmsConfig
{
	private MessageBirdConfig messagebird;

	private SqsConfig sqs;

	@Bean
	@Scope("prototype")
	public SqsClient sqsClient()
	{
		SqsClientBuilder sqsClientBuilder = SqsClient.builder()
			.region(Region.EU_WEST_1)
			.credentialsProvider(StaticCredentialsProvider.create(getAwsBasicCredentials()));
			if (StringUtils.isNotBlank(sqs.getEndpointOverride()))
			{
				sqsClientBuilder.endpointOverride(URI.create(sqs.getEndpointOverride()));
			}
		return sqsClientBuilder.build();
	}

	private AwsBasicCredentials getAwsBasicCredentials()
	{
		if (StringUtils.isNotBlank(sqs.getUsername()) && StringUtils.isNotBlank(sqs.getPassword()))
		{
			return AwsBasicCredentials.create(
				sqs.getUsername(),
				sqs.getPassword());
		}
		throw new IllegalStateException("Geen AWS credentials meegegeven");
	}

	@Bean
	public MessageBirdConfig messageBirdConfig()
	{
		return messagebird;
	}

	@Bean
	public String queueName()
	{
		return sqs == null ? null : sqs.getQueueName();
	}
}
