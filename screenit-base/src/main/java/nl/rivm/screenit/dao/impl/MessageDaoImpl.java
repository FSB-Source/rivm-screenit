package nl.rivm.screenit.dao.impl;

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

import java.util.List;

import nl.rivm.screenit.dao.MessageDao;
import nl.rivm.screenit.model.messagequeue.Message;
import nl.rivm.screenit.model.messagequeue.MessageType;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MessageDaoImpl extends AbstractAutowiredDao implements MessageDao
{
	@Override
	public Message getOldestMessage(MessageType type)
	{
		Criteria criteria = createMessageCriteria(type, null, 1);
		return (Message) criteria.uniqueResult();
	}

	@Override
	public List<Message> fetchMessages(MessageType type, String context, int maxFetchSize)
	{
		return createMessageCriteria(type, context, maxFetchSize).list();
	}

	@Override
	public Long fetchQueueSize(MessageType type, String context)
	{
		Criteria criteria = createMessageCriteria(type, context);
		criteria.setProjection(Projections.rowCount());
		return (Long) criteria.uniqueResult();
	}

	private Criteria createMessageCriteria(MessageType type, String context, int maxFetchSize)
	{
		Criteria criteria = createMessageCriteria(type, context);
		criteria.addOrder(Order.asc("id"));
		criteria.setMaxResults(maxFetchSize);
		return criteria;
	}

	private Criteria createMessageCriteria(MessageType type, String context)
	{
		Criteria criteria = getSession().createCriteria(Message.class);
		criteria.add(Restrictions.eq("type", type));
		if (context != null)
		{
			criteria.add(Restrictions.eq("context", context));
		}
		return criteria;
	}
}
