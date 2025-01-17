package nl.rivm.screenit.service.aws.impl;

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

import java.util.List;
import java.util.Optional;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.aws.AwsSqsService;

import org.springframework.stereotype.Service;

import software.amazon.awssdk.services.sqs.SqsClient;
import software.amazon.awssdk.services.sqs.model.DeleteMessageRequest;
import software.amazon.awssdk.services.sqs.model.GetQueueUrlRequest;
import software.amazon.awssdk.services.sqs.model.Message;
import software.amazon.awssdk.services.sqs.model.ReceiveMessageRequest;
import software.amazon.awssdk.services.sqs.model.SendMessageBatchRequest;
import software.amazon.awssdk.services.sqs.model.SendMessageBatchRequestEntry;

@Slf4j
@Service
public class AwsSqsServiceImpl implements AwsSqsService
{
	@Override
	public SendMessageBatchRequestEntry maakMessageBatchRequestEntry(String jsonDto, Long requestEntryId)
	{
		return SendMessageBatchRequestEntry.builder().id("id" + requestEntryId).messageBody(jsonDto).build();
	}

	@Override
	public String getSqsQueueUrl(SqsClient sqsClient, String queueName)
	{
		return sqsClient.getQueueUrl(GetQueueUrlRequest.builder().queueName(queueName).build()).queueUrl();
	}

	@Override
	public void verstuurBatchNaarSqs(SqsClient sqsClient, String queueUrl, List<SendMessageBatchRequestEntry> entries)
	{
		if (!entries.isEmpty())
		{
			var sendMessageBatchRequest = SendMessageBatchRequest.builder()
				.queueUrl(queueUrl)
				.entries(entries)
				.build();
			sqsClient.sendMessageBatch(sendMessageBatchRequest);
			LOG.info("Batch naar SQS queue gestuurd met aantal entries: {}", entries.size());
		}
	}

	@Override
	public Optional<Message> haalEenBerichtUitSqsOp(SqsClient sqsClient, String queueName)
	{
		var receiveMessageRequest = getReceiveMessageRequestMax1Message(getSqsQueueUrl(sqsClient, queueName));
		var messages = sqsClient.receiveMessage(receiveMessageRequest).messages();
		return messages.isEmpty() ? Optional.empty() : Optional.of(messages.get(0));
	}

	@Override
	public void verwijderBerichtVanSqsQueue(SqsClient sqsClient, String receiptHandle, String queueName)
	{
		var deleteMessageRequest = DeleteMessageRequest.builder().queueUrl(getSqsQueueUrl(sqsClient, queueName))
			.receiptHandle(receiptHandle).build();
		sqsClient.deleteMessage(deleteMessageRequest);
	}

	private ReceiveMessageRequest getReceiveMessageRequestMax1Message(String queueUrl)
	{
		return ReceiveMessageRequest.builder()
			.queueUrl(queueUrl)
			.maxNumberOfMessages(1)
			.build();
	}
}
