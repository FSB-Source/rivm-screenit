package nl.rivm.screenit.batch.dao.impl;

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

import java.util.List;

import nl.rivm.screenit.batch.dao.MammaHL7v24SendBerichtenQueueDao;
import nl.rivm.screenit.model.mamma.MammaHL7v24Message;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class MammaHL7v24SendBerichtenQueueDaoImpl extends AbstractAutowiredDao implements MammaHL7v24SendBerichtenQueueDao
{

	@Override
	public List<MammaHL7v24Message> fetchMessageQueueForIMS(int maxResults)
	{
		Session session = getSession();
		Criteria criteria = session.createCriteria(MammaHL7v24Message.class);
		criteria.addOrder(Order.asc("id"));
		criteria.setMaxResults(maxResults);
		List<MammaHL7v24Message> list = criteria.list();
		return list;
	}

	@Override
	public Long fetchQueueSize()
	{
		Session session = getSession();
		Criteria crit = session.createCriteria(MammaHL7v24Message.class);
		crit.setProjection(Projections.rowCount());
		return (Long) crit.uniqueResult();
	}
}
