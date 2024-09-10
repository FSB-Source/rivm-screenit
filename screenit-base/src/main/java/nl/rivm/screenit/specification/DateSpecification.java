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
import java.util.Date;
import java.util.function.Function;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

import org.hibernate.query.criteria.internal.CriteriaBuilderImpl;
import org.hibernate.query.criteria.internal.compile.RenderingContext;
import org.hibernate.query.criteria.internal.expression.LiteralExpression;
import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.RangeSpecification.maakRangePredicates;

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

	public static <T> Specification<T> overlaptLocalDateTime(Range<LocalDateTime> range, Function<Root<T>, Path<Date>> databaseColumnStartRange,
		Function<Root<T>, Path<Date>> databaseColumnEndRange)
	{
		var newRange = DateUtil.toUtilDateRange(range);
		return (r, q, cb) -> maakRangePredicates(cb, r, newRange, databaseColumnEndRange, databaseColumnStartRange);
	}

	public static <T> Specification<T> overlaptLocalDate(Range<LocalDate> range, Function<Root<T>, Path<Date>> databaseColumnStartRange,
		Function<Root<T>, Path<Date>> databaseColumnEndRange)
	{
		return overlaptLocalDateTime(DateUtil.toLocalDateTimeRange(range), databaseColumnStartRange, databaseColumnEndRange);
	}

	public static <T> Specification<T> bevatLocalDateTime(Range<LocalDateTime> range, Function<Root<T>, Path<Date>> databaseColumn)
	{
		return (r, q, cb) -> bevatLocalDateTime(range, databaseColumn.apply(r)).withPath(cb, r);
	}

	public static <T> PathAwarePredicate<T> bevatLocalDateTime(Range<LocalDateTime> range, Path<Date> databaseColumn)
	{
		var newRange = DateUtil.toUtilDateRange(range);
		return bevat(newRange, databaseColumn);
	}

	public static <T> Specification<T> bevatLocalDate(Range<LocalDate> range, Function<Root<T>, Path<Date>> databaseColumn)
	{
		return (r, q, cb) -> bevatLocalDate(range, databaseColumn.apply(r)).withPath(cb, r);
	}

	public static <T> PathAwarePredicate<T> bevatLocalDate(Range<LocalDate> range, Path<Date> databaseColumn)
	{
		return bevatLocalDateTime(DateUtil.toLocalDateTimeRange(range), databaseColumn);
	}

}
