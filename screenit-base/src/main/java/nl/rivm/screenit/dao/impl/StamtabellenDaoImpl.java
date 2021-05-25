
package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.StamtabellenDao;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Titel;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.CacheMode;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

@Repository
public class StamtabellenDaoImpl extends AbstractAutowiredDao implements StamtabellenDao
{

	@Override
	public List<Titel> getTitels()
	{
		Criteria criteria = getSession().createCriteria(Titel.class);

		criteria.add(Restrictions.eq("actief", Boolean.TRUE));
		criteria.addOrder(Order.asc("titel"));

		getSession().setCacheMode(CacheMode.NORMAL);

		return criteria.list();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Functie> getFuncties()
	{
		Criteria criteria = getSession().createCriteria(Functie.class);

		criteria.add(Restrictions.eq("actief", Boolean.TRUE));
		criteria.addOrder(Order.asc("functie"));

		getSession().setCacheMode(CacheMode.NORMAL);

		return criteria.list();
	}

	@Override
	public Functie getFunctie(String functie)
	{
		Criteria criteria = getSession().createCriteria(Functie.class);

		criteria.add(Restrictions.eq("functie", functie));
		criteria.add(Restrictions.eq("actief", Boolean.TRUE));

		getSession().setCacheMode(CacheMode.NORMAL);

		return (Functie) criteria.uniqueResult();
	}

}
