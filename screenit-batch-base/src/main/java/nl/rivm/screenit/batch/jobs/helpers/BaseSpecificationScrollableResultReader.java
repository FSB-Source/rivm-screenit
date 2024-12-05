package nl.rivm.screenit.batch.jobs.helpers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;

import nl.rivm.screenit.repository.impl.FluentJpaQueryImpl;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.hibernate.ScrollableResults;
import org.hibernate.StatelessSession;
import org.hibernate.internal.EmptyScrollableResults;
import org.springframework.data.jpa.domain.Specification;

public abstract class BaseSpecificationScrollableResultReader<T extends HibernateObject> extends BaseIdScrollableResultReader
{

	@Override
	protected ScrollableResults createScrollableResults(StatelessSession session)
	{
		var maxResults = getMaxResults();
		if (maxResults == 0)
		{
			return new EmptyScrollableResults();
		}

		var jpaQuery = new FluentJpaQueryImpl<>(createSpecification(), getHibernateSession(), getEntityClass(), getResultClass());
		jpaQuery.projections((cb, r) ->
		{
			var orders = getOrders(r, cb);
			jpaQuery.sortBy((r1, cb1) -> orders);
			return getProjections(orders, r, cb);
		});
		if (isDistinct())
		{
			jpaQuery.distinct();
		}

		return jpaQuery.setScrollFetchSize(fetchSize).scroll(maxResults);
	}

	@Override
	protected Long getScrollableResult(ScrollableResults scrollableResults)
	{
		var scrollableResult = scrollableResults.get(0);
		return scrollableResult instanceof Long ? (Long) scrollableResult : (Long) ((Object[]) scrollableResults.get()[0])[0];
	}

	protected abstract Specification<T> createSpecification();

	private List<Selection<?>> getProjections(List<Order> orders, Root<T> r, CriteriaBuilder cb)
	{
		List<Selection<?>> selections = new ArrayList<>(List.of(createProjection(r, cb)));
		if (isDistinct())
		{
			selections.addAll(orders.stream()
				.filter(Objects::nonNull)
				.map(Order::getExpression)
				.collect(Collectors.toList()));
			selections = selections.stream().distinct().collect(Collectors.toList());
		}
		return selections;
	}

	protected Expression<Long> createProjection(Root<T> r, CriteriaBuilder cb)
	{
		return r.get(AbstractHibernateObject_.ID);
	}

	protected List<Order> getOrders(Root<T> r, CriteriaBuilder cb)
	{
		var order = getOrder(r, cb);
		return order == null ? List.of() : List.of(order);
	}

	protected Order getOrder(Root<T> r, CriteriaBuilder cb)
	{
		return null;
	}

	protected boolean isDistinct()
	{
		return true;
	}

	protected Class<T> getEntityClass()
	{
		return (Class<T>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
	}

	protected Class<?> getResultClass()
	{
		return Long.class;
	}

	protected int getMaxResults()
	{
		return -1;
	}

}
