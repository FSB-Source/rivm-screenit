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

import java.util.ArrayList;
import java.util.function.Function;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.util.Pair;

import com.google.common.collect.BoundType;
import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class RangeSpecification
{

	public static <T, V extends Comparable<?>> Specification<T> overlapt(Range<V> range, Function<Root<T>, Path<V>> databaseColumnStartRange,
		Function<Root<T>, Path<V>> databaseColumnEndRange)
	{
		return (r, q, cb) -> overlapt(range, databaseColumnStartRange.apply(r), databaseColumnEndRange.apply(r)).withPath(cb, r);
	}

	public static <T, V extends Comparable<?>> PathAwarePredicate<T> overlapt(Range<V> range, Path<V> databaseColumnStartRange, Path<V> databaseColumnEndRange)
	{
		return maakRangePredicates(range, databaseColumnEndRange, databaseColumnStartRange);
	}

	public static <T, V extends Comparable<?>> Specification<T> bevat(Function<Root<T>, Path<V>> databaseColumnStartRange, Function<Root<T>, Path<V>> databaseColumnEndRange,
		Pair<BoundType, BoundType> boundTypes, V waarde)
	{
		return (r, q, cb) -> bevat(databaseColumnStartRange.apply(r), databaseColumnEndRange.apply(r), boundTypes, waarde).withPath(cb, r);
	}

	public static <T, V extends Comparable<?>> PathAwarePredicate<T> bevat(Path<V> databaseColumnStartRange, Path<V> databaseColumnEndRange, Pair<BoundType, BoundType> boundTypes,
		V waarde)
	{
		var range = Range.range(waarde, boundTypes.getSecond(), waarde, boundTypes.getFirst());
		return maakRangePredicates(range, databaseColumnEndRange, databaseColumnStartRange);
	}

	public static <T, V extends Comparable<?>> Specification<T> bevat(Range<V> range, Function<Root<T>, Path<V>> databaseColumn)
	{
		return (r, q, cb) -> bevat(range, databaseColumn.apply(r)).withPath(cb, r);
	}

	public static <T, V extends Comparable<?>> PathAwarePredicate<T> bevat(Range<V> range, Path<V> databaseColumn)
	{
		return overlapt(range, databaseColumn, databaseColumn);
	}

	static <T, V extends Comparable<?>> Predicate maakRangePredicates(CriteriaBuilder cb, Root<T> r, Range<V> range,
		Function<Root<T>, Path<V>> databaseColumnStartRange, Function<Root<T>, Path<V>> databaseColumnEndRange)
	{
		return maakRangePredicates(cb, range, databaseColumnStartRange.apply(r), databaseColumnEndRange.apply(r));
	}

	static <T, V extends Comparable<?>> PathAwarePredicate<T> maakRangePredicates(Range<V> range, Path<V> databaseColumnStartRange, Path<V> databaseColumnEndRange)
	{
		return (cb, r) -> maakRangePredicates(cb, range, databaseColumnStartRange, databaseColumnEndRange);
	}

	private static <V extends Comparable> Predicate maakRangePredicates(CriteriaBuilder cb, Range<V> range, Path<V> databaseColumnStartRange, Path<V> databaseColumnEndRange)
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
