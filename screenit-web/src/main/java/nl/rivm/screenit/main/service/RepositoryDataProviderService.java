package nl.rivm.screenit.main.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.lang.reflect.ParameterizedType;
import java.util.List;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.query.QueryUtils;

import com.google.common.primitives.Ints;

public abstract class RepositoryDataProviderService<T extends AbstractHibernateObject, R extends JpaSpecificationExecutor<T>, F>
{
	@Autowired
	private SessionFactory sessionFactory;

	protected abstract Specification<T> getSpecification(F filter, Sort sortParam);

	public List<T> findPage(long first, long count, F filter, Sort sort)
	{
		var cb = sessionFactory.getCurrentSession().getCriteriaBuilder();
		var query = cb.createQuery(getEntityClass());
		var r = query.from(getEntityClass());

		query
			.where(getSpecification(filter, sort).toPredicate(r, query, cb))
			.orderBy(QueryUtils.toOrders(sort, r, cb));

		var hquery = sessionFactory.getCurrentSession().createQuery(query);

		if (first > -1)
		{
			hquery.setFirstResult(Ints.checkedCast(first));
		}

		if (count > -1)
		{
			hquery.setMaxResults(Ints.checkedCast(count));
		}

		return hquery.getResultList();
	}

	public long size(F filter)
	{
		return getRepository().count(getSpecification(filter, Sort.unsorted()));
	}

	private R getRepository()
	{
		var clazz = (Class<R>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[1];
		return ApplicationContextProvider.getApplicationContext().getBean(clazz);
	}

	private Class<T> getEntityClass()
	{
		return (Class<T>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
	}
}
