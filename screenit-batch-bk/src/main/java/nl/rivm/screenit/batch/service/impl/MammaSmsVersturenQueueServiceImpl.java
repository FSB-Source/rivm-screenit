package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.config.MessageBirdConfig;
import nl.rivm.screenit.dto.SmsVersturenSqsDto;
import nl.rivm.screenit.service.aws.AwsSqsService;
import nl.rivm.screenit.service.mamma.MammaDigitaalContactService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.apache.commons.lang.StringUtils;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.google.common.collect.Lists;

import software.amazon.awssdk.services.sqs.SqsClient;
import software.amazon.awssdk.services.sqs.model.SendMessageBatchRequestEntry;

@Slf4j
@Configuration
@EnableScheduling
@AllArgsConstructor
public class MammaSmsVersturenQueueServiceImpl
{
	private MammaDigitaalContactService digitaalContactService;

	private AwsSqsService awsSqsService;

	private final String queueName;

	private MessageBirdConfig messageBirdConfig;

	private static final ObjectMapper mapper = new ObjectMapper();

	@Scheduled(cron = "0 */1 8-21 * * *")
	public void verstuurSmsBerichtenNaarQueue()
	{
		LOG.info("Running sms berichten herinnering queue");

		try (var sqsClient = ApplicationContextProvider.getApplicationContext().getBean(SqsClient.class))
		{
			String sqsQueueUrl = awsSqsService.getSqsQueueUrl(sqsClient, queueName);
			if (StringUtils.isNotBlank(sqsQueueUrl))
			{
				var afsprakenIds = digitaalContactService.getAfsprakenVoorSmsVersturen();

				var batch = Lists.partition(afsprakenIds, 10);
				int totaalSms = 0;
				for (List<Long> afsprakenPartitie : batch)
				{
					List<SendMessageBatchRequestEntry> entries = new ArrayList<>();
					List<Long> geslaagdeEntries = new ArrayList<>();

					for (Long afspraakId : afsprakenPartitie)
					{
						var smsSqsDTOOptional = digitaalContactService.maakSmsVersturenDTO(afspraakId);

						if (smsSqsDTOOptional.isPresent())
						{
							setCredentialsSmsSqs(smsSqsDTOOptional.get());
							String jsonDto = mapper.writeValueAsString(smsSqsDTOOptional.get());
							mapper.disable(SerializationFeature.INDENT_OUTPUT);
							entries.add(awsSqsService.maakMessageBatchRequestEntry(jsonDto, afspraakId));
							geslaagdeEntries.add(afspraakId);
						}
					}
					awsSqsService.verstuurBatchNaarSqs(sqsClient, sqsQueueUrl, entries);
					digitaalContactService.administreerSmsVerstuurd(geslaagdeEntries);

					totaalSms += entries.size();
				}
				LOG.info("Aantal: " + afsprakenIds.size() + " verwerkt, " + "Totaal: " + totaalSms + " verstuurd");
			}
		}
		catch (Exception e)
		{
			LOG.error("SMS versturen gefaald ", e);
		}
	}

	private void setCredentialsSmsSqs(SmsVersturenSqsDto smsSqsDTO)
	{
		smsSqsDTO.setCredentials(
			new SmsVersturenSqsDto.SmsCredentials(messageBirdConfig.getApplicatieNaam(), messageBirdConfig.getKlantId(), messageBirdConfig.getSmsSender()));
	}
}
