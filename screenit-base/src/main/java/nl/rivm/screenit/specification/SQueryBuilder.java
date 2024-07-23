package nl.rivm.screenit.specification;

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
import java.util.function.BiFunction;

import javax.persistence.EntityGraph;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;

import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.hibernate.SessionFactory;
import org.hibernate.query.Query;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.query.QueryUtils;

public class SQueryBuilder<S, F>
{
	private final CriteriaQuery<S> q;

	private final Root<F> r;

	private EntityGraph<S> entityGraph;

	private final SessionFactory sessionFactory;

	public static <F> SQueryBuilder<F, F> of(Class<F> fromClass)
	{
		return new SQueryBuilder(getSessionFactory(), fromClass, fromClass);
	}

	public static <S, F> SQueryBuilder<S, F> ofProjection(Class<S> selectClass, Class<F> fromClass,
		BiFunction<CriteriaBuilder, Root<F>, List<Selection<?>>> selectionFunction)
	{
		return new SQueryBuilder(getSessionFactory(), selectClass, fromClass, selectionFunction);
	}

	private SQueryBuilder(SessionFactory sessionFactory, Class<S> selectClass, Class<F> fromClass)
	{
		this(sessionFactory, selectClass, fromClass, (cb, r) -> List.of());
	}

	private SQueryBuilder(SessionFactory sessionFactory, Class<S> selectClass, Class<F> fromClass, BiFunction<CriteriaBuilder, Root<F>, List<Selection<?>>> selectionFunction)
	{
		this.sessionFactory = sessionFactory;
		var cb = sessionFactory.getCriteriaBuilder();
		q = cb.createQuery(selectClass);
		r = q.from(fromClass);

		var selections = selectionFunction.apply(cb, r);
		if (selections.size() > 1)
		{
			q.multiselect(selections);
		}
		else if (selections.size() == 1)
		{
			q.select((Selection<? extends S>) selections.get(0));
		}
		else
		{
			q.select((Selection<? extends S>) r);
		}
	}

	public SQueryBuilder<S, F> where(Specification<F> specification)
	{
		if (specification != null)
		{
			where(specification.toPredicate(r, q, sessionFactory.getCriteriaBuilder()));
		}
		return this;
	}

	public SQueryBuilder<S, F> where(Predicate predicate)
	{
		if (predicate != null)
		{
			q.where(predicate);
		}
		return this;
	}

	public EntityGraph<S> createEntityGraph()
	{
		entityGraph = sessionFactory.getCurrentSession().createEntityGraph(q.getResultType());
		return entityGraph;
	}

	public Query<S> createHQuery()
	{
		var hquery = sessionFactory.getCurrentSession().createQuery(q);
		if (entityGraph != null)
		{
			hquery.setHint("javax.persistence.fetchgraph", entityGraph);
		}
		return hquery;
	}

	public SQueryBuilder<S, F> orderBy(Sort sort)
	{
		if (sort != null)
		{
			q.orderBy(QueryUtils.toOrders(sort, r, sessionFactory.getCriteriaBuilder()));
		}
		return this;
	}

	private static @NotNull SessionFactory getSessionFactory()
	{
		return ApplicationContextProvider.getApplicationContext().getBean(SessionFactory.class);
	}
}
