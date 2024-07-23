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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.function.Function;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

import org.apache.commons.lang3.tuple.Pair;
import org.hibernate.query.criteria.internal.CriteriaBuilderImpl;
import org.hibernate.query.criteria.internal.compile.RenderingContext;
import org.hibernate.query.criteria.internal.expression.LiteralExpression;
import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.BoundType;
import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.RangeSpecification.maakRangePredicates;
import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;
import static nl.rivm.screenit.util.DateUtil.toUtilDate;
import static nl.rivm.screenit.util.RangeUtil.range;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class DateSpecification
{
	public static Expression<Date> truncate(String part, Path<Date> datePath, CriteriaBuilder cb)
	{
		return cb.function("date_trunc", Date.class, new LiteralExpression<>((CriteriaBuilderImpl) cb, part)
		{
			@Override
			public String render(RenderingContext renderingContext)
			{
				return "'" + part + "'";
			}
		}, datePath);
	}

	public static Expression<Integer> extractYear(Path<Date> datePath, CriteriaBuilder cb)
	{
		return cb.function("YEAR", Integer.class, datePath);
	}

	public static <T> Specification<T> overlaptLocalDateTime(Range<LocalDateTime> range, Function<Root<T>, Path<Date>> startPathFunction,
		Function<Root<T>, Path<Date>> endPathFunction)
	{
		var onderGrens = range.hasLowerBound() ? toUtilDate(range.lowerEndpoint()) : null;
		var onderGrensType = range.hasLowerBound() ? range.lowerBoundType() : null;
		var bovenGrens = range.hasUpperBound() ? toUtilDate(range.upperEndpoint()) : null;
		var bovenGrensType = range.hasUpperBound() ? range.upperBoundType() : null;

		var newRange = range(onderGrens, onderGrensType, bovenGrens, bovenGrensType);

		return (r, q, cb) -> maakRangePredicates(cb, r, newRange, endPathFunction, startPathFunction);
	}

	public static <T> Specification<T> bevatLocalDateTime(LocalDateTime datumTijd, Function<Root<T>, Path<Date>> startPathFunction,
		Function<Root<T>, Path<Date>> endPathFunction, Pair<BoundType, BoundType> boundTypes)
	{
		var newRange = range(toUtilDate(datumTijd), boundTypes.getLeft(), toUtilDate(datumTijd), boundTypes.getRight());
		return (r, q, cb) -> maakRangePredicates(cb, r, newRange, startPathFunction, endPathFunction);
	}

	public static <T> Specification<T> tussenLocalDateTime(Range<LocalDateTime> range, Function<Root<T>, Path<Date>> datePathFunction)
	{
		return (r, q, cb) -> overlaptLocalDateTime(range, datePathFunction, datePathFunction).toPredicate(r, q, cb);
	}

	@Deprecated
	public static <T> Specification<T> valtBinnenDatumRange(Range<Date> range, Function<Root<T>, Path<Date>> startPathFunction, Function<Root<T>, Path<Date>> endPathFunction)
	{
		return (r, q, cb) ->
		{
			var startProperty = startPathFunction.apply(r);
			var endProperty = endPathFunction.apply(r);
			return cb.and(cb.greaterThan(endProperty, range.lowerEndpoint()), cb.lessThan(startProperty, range.upperEndpoint()));
		};
	}

	@Deprecated
	public static <S> Specification<S> betweenDates(LocalDate vanaf, LocalDate totEnMet, Function<Root<S>, Path<Date>> pathSupplier)
	{
		return (r, q, cb) -> betweenDatesPredicate(vanaf, totEnMet).withPath(cb, pathSupplier.apply(r));
	}

	@Deprecated
	public static <S> Specification<S> betweenLocalDates(LocalDate vanaf, LocalDate totEnMet, Function<Root<S>, Path<LocalDate>> pathSupplier)
	{
		return (r, q, cb) -> betweenLocalDatesPredicate(vanaf, totEnMet).withPath(cb, pathSupplier.apply(r));
	}

	@Deprecated
	public static PathAwarePredicate<Date> betweenDatesPredicate(Range<LocalDate> range)
	{
		return betweenDatesPredicate(range.lowerEndpoint(), range.upperEndpoint());
	}

	@Deprecated
	public static PathAwarePredicate<Date> betweenDatesPredicate(LocalDate vanaf, LocalDate totEnMet)
	{
		return (cb, r) ->
		{
			var predicates = new ArrayList<Predicate>();

			if (vanaf != null)
			{
				predicates.add(cb.greaterThanOrEqualTo(r, toUtilDate(vanaf)));
			}

			if (totEnMet != null)
			{
				predicates.add(cb.lessThan(r, toUtilDate(totEnMet.plusDays(1))));
			}

			return composePredicates(cb, predicates);
		};
	}

	@Deprecated
	public static PathAwarePredicate<LocalDate> betweenLocalDatesPredicate(LocalDate vanaf, LocalDate totEnMet)
	{
		return (cb, r) ->
		{
			var predicates = new ArrayList<Predicate>();

			if (vanaf != null)
			{
				predicates.add(cb.greaterThanOrEqualTo(r, vanaf));
			}

			if (totEnMet != null)
			{
				predicates.add(cb.lessThan(r, totEnMet.plusDays(1)));
			}

			return composePredicates(cb, predicates);
		};
	}

}
