package nl.rivm.screenit.dao.colon.impl;

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

import java.util.Date;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.dao.colon.ComplicatieDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.Complicatie;
import nl.rivm.screenit.model.SortState;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class ComplicatieDaoImpl extends AbstractAutowiredDao implements ComplicatieDao
{

	@Override
	public Iterator<Complicatie> getComplicaties(Complicatie searchObject, List<Long> hierarchieCriteria, int first, int count, SortState<String> sortState)
	{
		Criteria crit = createBasicComplicatieCriteria(searchObject, hierarchieCriteria);
		if (sortState.isAsc())
		{
			crit.addOrder(Order.asc(sortState.getSortParam()));
		}
		else
		{
			crit.addOrder(Order.desc(sortState.getSortParam()));
		}

		crit.setMaxResults(count);
		crit.setFirstResult(first);

		return crit.list().iterator();
	}

	@Override
	public long countComplicaties(Complicatie searchObject, List<Long> hierarchieCriteria)
	{
		Criteria crit = createBasicComplicatieCriteria(searchObject, hierarchieCriteria);
		return crit.list().size();
	}

	private Criteria createBasicComplicatieCriteria(Complicatie searchObject, List<Long> hierarchieCriteria)
	{
		Criteria criteria = this.getSession().createCriteria(Complicatie.class);
		criteria.createAlias("instellingGebruiker", "instellingGebruiker");
		criteria.createAlias("instellingGebruiker.organisatie", "organisatie");

		criteria.add(Restrictions.eq("client", searchObject.getClient()));
		criteria.add(Restrictions.eq("actief", true));

		criteria.add(Restrictions.in("organisatie.id", hierarchieCriteria.toArray()));

		return criteria;
	}

	@Override
	public List<Complicatie> geefAlleNietGekoppeldeComplicaties(Client client, Date date)
	{
		BaseCriteria<Complicatie> criteria = new BaseCriteria<Complicatie>(Complicatie.class);
		criteria.add(Restrictions.eq("client", client));
		criteria.add(Restrictions.isNull("mdlverslag"));
		criteria.add(Restrictions.eq("handmatig", Boolean.TRUE));
		criteria.add(Restrictions.ge("datum", date));
		return criteria.list(getSession());
	}

}
