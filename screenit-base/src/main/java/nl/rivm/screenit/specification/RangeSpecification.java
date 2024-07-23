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

	public static <T, D extends Comparable<?>> Specification<T> overlapt(Range<D> range, Function<Root<T>, Path<D>> startPathFunction,
		Function<Root<T>, Path<D>> endPathFunction)
	{
		return (r, q, cb) -> overlapt(range, startPathFunction.apply(r), endPathFunction.apply(r)).withPath(cb, r);
	}

	public static <T, D extends Comparable<?>> PathAwarePredicate<T> overlapt(Range<D> range, Path<D> startPath, Path<D> endPath)
	{
		return maakRangePredicates(range, endPath, startPath);
	}

	public static <T, D extends Comparable<?>> Specification<T> bevat(D value, Function<Root<T>, Path<D>> startPathFunction, Function<Root<T>, Path<D>> endPathFunction,
		Pair<BoundType, BoundType> boundTypes)
	{
		return (r, q, cb) -> bevat(value, startPathFunction.apply(r), endPathFunction.apply(r), boundTypes).withPath(cb, r);
	}

	public static <T, D extends Comparable<?>> PathAwarePredicate<T> bevat(D value, Path<D> startPath, Path<D> endPath,		Pair<BoundType, BoundType> boundTypes)
	{
		var range = Range.range(value, boundTypes.getFirst(), value, boundTypes.getSecond());
		return maakRangePredicates(range, startPath, endPath);
	}

	public static <T, D extends Comparable<?>> Specification<T> tussen(Range<D> range, Function<Root<T>, Path<D>> pathFunction)
	{
		return (r, q, cb) -> tussen(range, pathFunction.apply(r)).withPath(cb, r);
	}

	public static <T, D extends Comparable<?>> PathAwarePredicate<T> tussen(Range<D> range, Path<D> path)
	{
		return overlapt(range, path, path);
	}

	static <T, D extends Comparable<?>> Predicate maakRangePredicates(CriteriaBuilder cb, Root<T> r, Range<D> range,
		Function<Root<T>, Path<D>> startPathFunction, Function<Root<T>, Path<D>> endPathFunction)
	{
		return maakRangePredicates(cb, range, startPathFunction.apply(r), endPathFunction.apply(r));
	}

	static <T, D extends Comparable<?>> PathAwarePredicate<T> maakRangePredicates(Range<D> range, Path<D> startProperty, Path<D> endProperty)
	{
		return (cb, r) -> maakRangePredicates(cb, range, startProperty, endProperty);
	}

	private static <D extends Comparable> Predicate maakRangePredicates(CriteriaBuilder cb, Range<D> range, Path<D> startProperty, Path<D> endProperty)
	{
		var predicates = new ArrayList<Predicate>();

		if (range != null)
		{
			if (range.hasLowerBound())
			{
				if (range.lowerBoundType() == BoundType.CLOSED)
				{
					predicates.add(cb.greaterThanOrEqualTo(startProperty, range.lowerEndpoint()));
				}
				else if (range.lowerBoundType() == BoundType.OPEN)
				{
					predicates.add(cb.greaterThan(startProperty, range.lowerEndpoint()));
				}
			}
			if (range.hasUpperBound())
			{
				if (range.upperBoundType() == BoundType.CLOSED)
				{
					predicates.add(cb.lessThanOrEqualTo(endProperty, range.upperEndpoint()));
				}
				else if (range.upperBoundType() == BoundType.OPEN)
				{
					predicates.add(cb.lessThan(endProperty, range.upperEndpoint()));
				}
			}
		}
		return composePredicates(cb, predicates);
	}
}
