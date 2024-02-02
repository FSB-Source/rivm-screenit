
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

import nl.rivm.screenit.dao.VragenlijstBaseDao;
import nl.rivm.screenit.model.vragenlijsten.Vragenlijst;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

import com.google.common.primitives.Ints;

@Repository
public class VragenlijstBaseDaoImpl extends AbstractAutowiredDao implements VragenlijstBaseDao
{

	@SuppressWarnings("unchecked")
	@Override
	public <T extends Vragenlijst> List<T> searchVragenlijsten(T searchObject, long first, long count, String sortProperty, boolean asc)
	{
		Criteria criteria = this.getSession().createCriteria(searchObject.getClass());

		if (searchObject.getActief() != null)
		{
			criteria.add(Restrictions.eq("actief", searchObject.getActief()));
		}

		if (first >= 0)
		{
			criteria.setFirstResult(Ints.checkedCast(first));
			criteria.setMaxResults(Ints.checkedCast(count));
		}

		if (asc)
		{
			criteria.addOrder(Order.asc(sortProperty));
		}
		else
		{
			criteria.addOrder(Order.desc(sortProperty));
		}

		return criteria.list();
	}

	@Override
	public <T extends Vragenlijst> long countVragenlijsten(T searchObject)
	{
		Criteria criteria = this.getSession().createCriteria(searchObject.getClass());
		if (searchObject.getActief() != null)
		{
			criteria.add(Restrictions.eq("actief", searchObject.getActief()));
		}
		criteria.setProjection(Projections.rowCount());
		return ((Number) criteria.uniqueResult()).longValue();
	}

}
