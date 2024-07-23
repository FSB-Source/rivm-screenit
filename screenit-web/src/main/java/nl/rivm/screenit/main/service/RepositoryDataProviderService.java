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
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.main.util.WicketSpringDataUtil;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.hibernate.SessionFactory;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.query.QueryUtils;

import com.google.common.primitives.Ints;

public abstract class RepositoryDataProviderService<T extends HibernateObject, R extends JpaSpecificationExecutor<T>, F>
{
	@Autowired
	private SessionFactory sessionFactory;

	protected abstract Specification<T> getSpecification(F filter, Sort sortParam);

	public List<T> findPage(long first, long count, F filter, SortParam<String> sortParam)
	{
		var sort = WicketSpringDataUtil.toSpringSort(sortParam);
		return findPage(first, count, filter, sort);
	}

	public List<T> findAll(F filter)
	{
		var cb = sessionFactory.getCurrentSession().getCriteriaBuilder();
		var query = cb.createQuery(getEntityClass());
		var r = query.from(getEntityClass());
		var specification = getSpecification(filter, null).toPredicate(r, query, cb);
		if (specification != null)
		{
			query.where(specification);
		}

		return sessionFactory.getCurrentSession().createQuery(query).getResultList();
	}

	public List<T> findPage(long first, long count, F filter, Sort sort)
	{
		var cb = sessionFactory.getCurrentSession().getCriteriaBuilder();
		var query = cb.createQuery(getEntityClass());
		var r = query.from(getEntityClass());
		var specification = getSpecification(filter, sort).toPredicate(r, query, cb);
		if (specification != null)
		{
			query.where(specification);
		}
		query.orderBy(getOrders(sort, r, cb));

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

	private @NotNull List<Order> getOrders(Sort sort, Root<T> r, CriteriaBuilder cb)
	{
		var orders = new ArrayList<Order>();
		var splittedSortOrders = new ArrayList<Sort.Order>();
		for (var order : sort)
		{
			var correctedOrder = addJoinsForSortingOrCreateDedicatedOrders(order, r, cb);
			if (correctedOrder == null)
			{
				splittedSortOrders.add(order);
			}
			else
			{
				orders.addAll(QueryUtils.toOrders(Sort.by(splittedSortOrders), r, cb));
				orders.add(correctedOrder);
				splittedSortOrders.clear();
			}
		}
		orders.addAll(QueryUtils.toOrders(Sort.by(splittedSortOrders), r, cb));
		return orders;
	}

	protected Order addJoinsForSortingOrCreateDedicatedOrders(Sort.Order order, Root<T> r, CriteriaBuilder cb)
	{
		return null; 
	}

	protected Sort getSort(Sort sort)
	{
		return sort;
	}

	public long size(F filter)
	{
		return getRepository().count(getSpecification(filter, Sort.unsorted()));
	}

	private R getRepository()
	{
		var clazz = (Class<R>) getActualTypeArguments()[1];
		return ApplicationContextProvider.getApplicationContext().getBean(clazz);
	}

	private Class<T> getEntityClass()
	{
		return (Class<T>) getActualTypeArguments()[0];
	}

	protected Type[] getActualTypeArguments()
	{
		return ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments();
	}
}
