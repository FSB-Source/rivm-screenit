package nl.rivm.screenit.service.impl;

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

import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.MessageDao;
import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MessageService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@Slf4j
@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MessageServiceImpl implements MessageService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MessageDao messageDao;

	@Autowired
	@Qualifier(value = "applicationInstance")
	private String applicationInstance;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private static final ObjectMapper objectMapper = new ObjectMapper();

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void queueMessage(MessageType type, Object content)
	{
		queueMessage(type, content, null);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
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
			hibernateService.saveOrUpdate(newMessage);
		}
		catch (JsonProcessingException e)
		{
			throw new IllegalArgumentException(e.getMessage(), e);
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void dequeueMessage(Message message)
	{
		hibernateService.delete(message);
	}

	@Override
	public Message getOldestMessage(MessageType type)
	{
		return messageDao.getOldestMessage(type);
	}

	@Override
	public List<Message> fetchMessages(MessageType type, int maxFetchSize)
	{
		return fetchMessages(type, null, maxFetchSize);
	}

	@Override
	public List<Message> fetchMessages(MessageType type, String context, int maxFetchSize)
	{
		return messageDao.fetchMessages(type, context, maxFetchSize);
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
	public Long fetchQueueSize(MessageType type)
	{
		return fetchQueueSize(type, null);
	}

	@Override
	public Long fetchQueueSize(MessageType type, String context)
	{
		return messageDao.fetchQueueSize(type, context);
	}

}
