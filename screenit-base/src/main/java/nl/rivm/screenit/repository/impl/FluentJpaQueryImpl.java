package nl.rivm.screenit.repository.impl;

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
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Consumer;

import javax.persistence.EntityGraph;
import javax.persistence.EntityManager;
import javax.persistence.Tuple;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;

import nl.rivm.screenit.repository.FluentJpaQuery;

import org.springframework.dao.IncorrectResultSizeDataAccessException;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.query.QueryUtils;
import org.springframework.util.Assert;

class FluentJpaQueryImpl<T, P> implements FluentJpaQuery<T, P>
{
	private final Specification<T> specification;

	private final EntityManager entityManager;

	private final Class<P> projectionType;

	private final Class<T> entityType;

	private boolean distinctOn;

	private BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> projectionFunction;

	private Sort sort = Sort.unsorted();

	private EntityGraph<T> entityGraph;

	public FluentJpaQueryImpl(Specification<T> specification, EntityManager entityManager, Class<T> entityType, Class<P> projectionType)
	{
		this.specification = specification;
		this.entityManager = entityManager;
		this.projectionType = projectionType;
		this.entityType = entityType;
	}

	@Override
	public FluentJpaQuery<T, P> sortBy(Sort sort)
	{
		this.sort = this.sort.and(sort);
		return this;
	}

	@Override
	public FluentJpaQuery<T, P> projection(BiFunction<CriteriaBuilder, Root<T>, Selection<?>> projectionFunction)
	{
		Assert.isNull(this.projectionFunction, "Only one of projection() or projections() may be called and only a single time. Use projections() for multiple projections.");
		this.projectionFunction = (cb, r) -> List.of(projectionFunction.apply(cb, r));
		return this;
	}

	@Override
	public FluentJpaQuery<T, P> projections(BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> projectionFunction)
	{
		Assert.isNull(this.projectionFunction, "Only one of projection() or projections() may be called and only a single time.");
		this.projectionFunction = projectionFunction;
		return this;
	}

	@Override
	public FluentJpaQuery<T, P> distinct(boolean distinctOn)
	{
		this.distinctOn = distinctOn;
		return this;
	}

	@Override
	public FluentJpaQuery<T, P> fetch(Consumer<EntityGraph<T>> entityGraphFunction)
	{
		if (entityGraph == null)
		{
			entityGraph = entityManager.createEntityGraph(entityType);
		}

		entityGraphFunction.accept(this.entityGraph);

		return this;
	}

	@Override
	public List<P> all()
	{
		return createTypedQuery().getResultList();
	}

	@Override
	public Optional<P> first()
	{
		Assert.isTrue(sort.isSorted(), "Sorting is required for first()");
		return createTypedQuery()
			.setMaxResults(1)
			.getResultList()
			.stream()
			.findFirst();
	}

	@Override
	public Optional<P> one()
	{
		var results = createTypedQuery()
			.setMaxResults(2)
			.getResultList();

		if (results.size() > 1)
		{
			throw new IncorrectResultSizeDataAccessException(1);
		}
		return results.stream().findFirst();

	}

	private TypedQuery<P> createTypedQuery()
	{
		var cb = entityManager.getCriteriaBuilder();
		var q = cb.createQuery(projectionType);
		var r = q.from(entityType);

		addWhereClause(r, q, cb);
		addSelections(r, q, cb); 
		addOrderBy(r, q, cb);
		q.distinct(distinctOn);

		var typedQuery = entityManager.createQuery(q);
		addFetchGraph(typedQuery);
		return typedQuery;
	}

	private void addSelections(Root<T> r, CriteriaQuery<P> q, CriteriaBuilder cb)
	{
		if (projectionFunction != null)
		{
			var selections = projectionFunction.apply(cb, r).toArray(Selection<?>[]::new);

			if (selections.length == 1)
			{
				q.select((Selection<? extends P>) selections[0]);
			}
			else if (projectionType.isArray() || projectionType == Tuple.class)
			{
				q.multiselect(selections);
			}
			else
			{
				q.select(cb.construct(projectionType, selections));
			}
		}
	}

	private void addWhereClause(Root<T> r, CriteriaQuery<P> q, CriteriaBuilder cb)
	{
		if (specification != null)
		{
			q.where(specification.toPredicate(r, q, cb));
		}
	}

	private void addOrderBy(Root<T> r, CriteriaQuery<P> q, CriteriaBuilder cb)
	{
		if (sort.isSorted())
		{
			q.orderBy(QueryUtils.toOrders(sort, r, cb));
		}
	}

	private void addFetchGraph(TypedQuery<P> typedQuery)
	{
		if (entityGraph != null)
		{
			typedQuery.setHint("javax.persistence.fetchgraph", entityGraph);
		}
	}
}
