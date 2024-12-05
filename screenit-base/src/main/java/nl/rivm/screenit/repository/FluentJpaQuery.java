package nl.rivm.screenit.repository;

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
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;

import nl.rivm.screenit.util.functionalinterfaces.TriFunction;

import org.hibernate.ScrollableResults;
import org.springframework.data.domain.Sort;

public interface FluentJpaQuery<T, P>
{
	FluentJpaQuery<T, P> sortBy(Sort sort);

	FluentJpaQuery<T, P> sortBy(Sort sort, TriFunction<Sort.Order, Root<T>, CriteriaBuilder, Order> sortFunction);

	FluentJpaQuery<T, P> sortBy(BiFunction<Root<T>, CriteriaBuilder, List<Order>> ordersFunction);

	FluentJpaQuery<T, P> projection(BiFunction<CriteriaBuilder, Root<T>, Selection<?>> projectionFunction);

	FluentJpaQuery<T, P> projections(BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> projectionFunction);

	FluentJpaQuery<T, P> groupBy(BiFunction<CriteriaBuilder, Root<T>, List<Expression<?>>> projectionFunction);

	FluentJpaQuery<T, P> fetch(Consumer<EntityGraph<T>> entityGraphFunction);

	FluentJpaQuery<T, P> distinct();

	FluentJpaQuery<T, P> setScrollFetchSize(int fetchSize);

	List<P> all();

	List<P> all(long first, long count);

	Optional<P> first();

	Optional<P> one();

	ScrollableResults scroll(Integer maxResults);

}
