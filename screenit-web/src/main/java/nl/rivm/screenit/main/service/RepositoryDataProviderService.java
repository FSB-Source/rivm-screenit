package nl.rivm.screenit.main.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import javax.persistence.EntityGraph;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.main.util.WicketSpringDataUtil;
import nl.rivm.screenit.repository.impl.FluentJpaQueryImpl;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

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
		return findPage(-1, -1, filter, Sort.unsorted());
	}

	public List<T> findPage(long first, long count, F filter, Sort sort)
	{
		var jpaQuery = new FluentJpaQueryImpl<>(getSpecification(filter, sort), sessionFactory.getCurrentSession(), getEntityClass(), getEntityClass());
		jpaQuery.sortBy(getSort(sort), this::addJoinsForSortingOrCreateDedicatedOrders);
		jpaQuery.fetch(this::fetch);

		return jpaQuery.all(first, count);
	}

	protected void fetch(EntityGraph<T> entityGraph)
	{
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

	protected R getRepository()
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
