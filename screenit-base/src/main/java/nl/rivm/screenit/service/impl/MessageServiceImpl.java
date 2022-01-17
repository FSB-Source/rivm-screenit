package nl.rivm.screenit.service.impl;

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

import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MessageService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service
@Transactional(propagation= Propagation.REQUIRED)
public class MessageServiceImpl implements MessageService
{
	private static final Logger LOG = LoggerFactory.getLogger(MessageServiceImpl.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	@Qualifier(value = "applicatieInstantie")
	private String applicationInstance;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	private final static ObjectMapper objectMapper = new ObjectMapper();

	@Override
	public void queueMessage(MessageType type, Object content) throws JsonProcessingException
	{
		Message newMessage = new Message();
		newMessage.setType(type);
		newMessage.setSenderApplicationInstance(applicationInstance);
		newMessage.setContent(objectMapper.writeValueAsString(content));
		newMessage.setAanmaakMoment(currentDateSupplier.getDate());
		hibernateService.saveOrUpdate(newMessage);
	}

	@Override
	public void dequeueMessage(Message message)
	{
		hibernateService.delete(message);
	}

	@Override
	@Transactional(propagation= Propagation.SUPPORTS, readOnly = true)
	public Message getOldestMessage(MessageType type)
	{
		Criteria criteria = hibernateService.getHibernateSession().createCriteria(Message.class);
		criteria.add(Restrictions.eq("type", type));
		criteria.addOrder(Order.asc("id"));
		criteria.setMaxResults(1);
		return (Message) criteria.uniqueResult();
	}

	@Override
	public <T> T getContent(Message message)
	{
		Object contentDto = null;
		if (message != null)
		{
			String content = message.getContent();
			try
			{
				contentDto = objectMapper.readValue(message.getContent(), message.getType().getContentType());
			}
			catch (JsonProcessingException e)
			{
				LOG.error("Fout bij conversie naar DTO", e);
			}
		}
		return (T) contentDto;
	}

}
