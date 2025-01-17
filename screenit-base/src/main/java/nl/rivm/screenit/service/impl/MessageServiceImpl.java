package nl.rivm.screenit.service.impl;

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

import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.model.messagequeue.Message_;
import nl.rivm.screenit.repository.algemeen.MessageRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MessageService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import static nl.rivm.screenit.specification.algemeen.MessageSpecification.filterContext;
import static nl.rivm.screenit.specification.algemeen.MessageSpecification.heeftType;

@Slf4j
@Service
public class MessageServiceImpl implements MessageService
{
	@Autowired
	@Qualifier(value = "applicationInstance")
	private String applicationInstance;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private MessageRepository messageRepository;

	private static final ObjectMapper objectMapper = new ObjectMapper();

	@Override
	@Transactional
	public void queueMessage(MessageType type, Object content)
	{
		queueMessage(type, content, null);
	}

	@Override
	@Transactional
	public void queueMessage(MessageType type, Object content, String context)
	{
		try
		{
			Message newMessage = new Message();
			newMessage.setType(type);
			newMessage.setSenderApplicationInstance(applicationInstance);
			newMessage.setContent(objectMapper.writeValueAsString(content));
			newMessage.setAanmaakMoment(currentDateSupplier.getDate());
			newMessage.setContext(context);
			messageRepository.save(newMessage);
		}
		catch (JsonProcessingException e)
		{
			throw new IllegalArgumentException(e.getMessage(), e);
		}
	}

	@Override
	@Transactional
	public void dequeueMessage(Message message)
	{
		messageRepository.delete(message);
	}

	@Override
	public Optional<Message> getOldestMessage(MessageType type)
	{
		return messageRepository.findFirst(heeftType(type), Sort.by(Sort.Order.asc(Message_.ID)));
	}

	@Override
	public List<Message> fetchMessages(MessageType type, String context, int maxFetchSize)
	{
		return messageRepository.findAll(heeftType(type).and(filterContext(context)), PageRequest.of(0, maxFetchSize, Sort.by(Sort.Order.asc(Message_.ID)))).getContent();
	}

	@Override
	public <T> T getContent(Message message) throws JsonProcessingException
	{
		Object contentDto = null;
		if (message != null)
		{
			contentDto = objectMapper.readValue(message.getContent(), message.getType().getContentType());
		}
		return (T) contentDto;
	}

	@Override
	public Long fetchQueueSize(MessageType type, String context)
	{
		return messageRepository.count(heeftType(type).and(filterContext(context)));
	}
}
