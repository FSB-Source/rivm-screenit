package nl.rivm.screenit.main.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.SortState;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.wicket.extensions.markup.html.repeater.data.sort.ISortState;
import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
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

	protected abstract Specification<T> getSpecification(F filter);

	public List<T> findPage(long first, long count, F filter, SortParam<String> sortParam)
	{
		var cb = sessionFactory.getCurrentSession().getCriteriaBuilder();
		var query = cb.createQuery(getEntityClass());
		var r = query.from(getEntityClass());
		var sort = Sort.by(sortParam.isAscending() ? Sort.Direction.ASC : Sort.Direction.DESC, sortParam.getProperty());

		query
			.where(getSpecification(filter).toPredicate(r, query, cb))
			.orderBy(QueryUtils.toOrders(sort, r, cb));

		var hquery = sessionFactory.getCurrentSession().createQuery(query);
		hquery.setFirstResult(Ints.checkedCast(first));
		hquery.setMaxResults(Ints.checkedCast(count));

		return hquery.getResultList();
	}

	public long size(F filter)
	{
		return getRepository().count(getSpecification(filter));
	}

	private R getRepository()
	{
		var clazz = (Class<R>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[1];
		return SpringBeanProvider.getInstance().getBean(clazz);
	}

	private Class<T> getEntityClass()
	{
		return (Class<T>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
	}
}
