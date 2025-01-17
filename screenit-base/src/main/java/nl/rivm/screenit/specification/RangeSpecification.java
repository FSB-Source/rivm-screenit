package nl.rivm.screenit.specification;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;
import java.util.function.Function;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Predicate;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.util.functionalinterfaces.TriFunction;

import org.springframework.data.util.Pair;

import com.google.common.collect.BoundType;
import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class RangeSpecification
{

	public static <T, V extends Comparable<?>> ExtendedSpecification<T> overlapt(Range<V> range,
		Function<From<?, ? extends T>, Expression<V>> databaseColumnStartRange,
		Function<From<?, ? extends T>, Expression<V>> databaseColumnEndRange)
	{
		return (r, q, cb) -> maakRangePredicates(cb, range, databaseColumnEndRange.apply(r), databaseColumnStartRange.apply(r));
	}

	public static <T, V extends Comparable<?>> ExtendedSpecification<T> bevat(Function<From<?, ? extends T>, Expression<V>> databaseColumnStartRange,
		Function<From<?, ? extends T>, Expression<V>> databaseColumnEndRange,
		Pair<BoundType, BoundType> boundTypes, V waarde)
	{
		return (r, q, cb) -> bevatPredicate(cb, databaseColumnStartRange.apply(r), databaseColumnEndRange.apply(r), boundTypes, waarde);
	}

	public static <T, V extends Comparable<?>> ExtendedSpecification<T> bevat(
		TriFunction<From<?, ? extends T>, CriteriaQuery<?>, CriteriaBuilder, Expression<V>> databaseColumnStartRange,
		TriFunction<From<?, ? extends T>, CriteriaQuery<?>, CriteriaBuilder, Expression<V>> databaseColumnEndRange,
		Pair<BoundType, BoundType> boundTypes, V waarde)
	{
		return (r, q, cb) -> bevatPredicate(cb, databaseColumnStartRange.apply(r, q, cb), databaseColumnEndRange.apply(r, q, cb), boundTypes, waarde);
	}

	private static <V extends Comparable<?>> Predicate bevatPredicate(CriteriaBuilder cb, Expression<V> start, Expression<V> end, Pair<BoundType, BoundType> boundTypes, V waarde)
	{
		var range = Range.range(waarde, boundTypes.getSecond(), waarde, boundTypes.getFirst());
		return maakRangePredicates(cb, range, end, start);
	}

	public static <T, V extends Comparable<?>> ExtendedSpecification<T> bevat(Range<V> range, Function<From<?, ? extends T>, Expression<V>> databaseColumn)
	{
		return overlapt(range, databaseColumn, databaseColumn);
	}

	static <V extends Comparable> Predicate maakRangePredicates(CriteriaBuilder cb, Range<V> range, Expression<V> databaseColumnStartRange,
		Expression<V> databaseColumnEndRange)
	{
		var predicates = new ArrayList<Predicate>();

		if (range != null)
		{
			if (range.hasLowerBound())
			{
				if (range.lowerBoundType() == BoundType.CLOSED)
				{
					predicates.add(cb.greaterThanOrEqualTo(databaseColumnStartRange, range.lowerEndpoint()));
				}
				else if (range.lowerBoundType() == BoundType.OPEN)
				{
					predicates.add(cb.greaterThan(databaseColumnStartRange, range.lowerEndpoint()));
				}
			}
			if (range.hasUpperBound())
			{
				if (range.upperBoundType() == BoundType.CLOSED)
				{
					predicates.add(cb.lessThanOrEqualTo(databaseColumnEndRange, range.upperEndpoint()));
				}
				else if (range.upperBoundType() == BoundType.OPEN)
				{
					predicates.add(cb.lessThan(databaseColumnEndRange, range.upperEndpoint()));
				}
			}
		}
		return composePredicates(cb, predicates);
	}
}
