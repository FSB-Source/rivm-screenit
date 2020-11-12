package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.OpenIdAssocStoreDao;
import nl.rivm.screenit.model.OpenIdAssociation;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

@Repository
public class OpenIdAssocStoreDaoImpl extends AbstractAutowiredDao implements OpenIdAssocStoreDao
{
	@Override
	public void saveAssoc(OpenIdAssociation assoc)
	{
		getSession().saveOrUpdate(assoc);
		getSession().flush();
	}

	@Override
	public OpenIdAssociation loadByHandle(String handle)
	{
		Criteria crit = getSession().createCriteria(OpenIdAssociation.class);
		crit.add(Restrictions.eq("handle", handle));
		Object result = crit.uniqueResult();
		if (result instanceof OpenIdAssociation)
		{
			return (OpenIdAssociation) result;
		}
		else
		{
			return null;
		}
	}

	@Override
	public int removeByHandle(String handle)
	{
		String hql = "DELETE FROM openidassociation WHERE handle = :handle";
		Query query = getSession().createQuery(hql);
		query.setString("handle", handle);
		return query.executeUpdate();
	}

	@Override
	public int cleanupByDate(Date date)
	{
		String hql = "DELETE FROM openidassociation WHERE expdate < :expdate";
		Query query = getSession().createQuery(hql);
		query.setDate("expdate", date);
		return query.executeUpdate();
	}
}
