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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.function.Function;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.From;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.util.DateUtil;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.RangeSpecification.maakRangePredicates;
import static nl.rivm.screenit.specification.RangeSpecification.overlapt;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class DateSpecification
{
	public static Expression<Date> truncate(String part, Expression<Date> datePath, CriteriaBuilder cb)
	{
		return cb.function("date_trunc", Date.class, new StringLiteral(cb, part), datePath);
	}

	public static Expression<Integer> extractYear(Expression<Date> datePath, CriteriaBuilder cb)
	{
		return cb.function("YEAR", Integer.class, datePath);
	}

	public static Expression<Date> intervalInDagen(CriteriaBuilder cb, Expression<Date> datePath, long dagen)
	{
		return cb.function("intervalInDagen", Date.class, cb.literal(dagen), datePath);
	}

	public static <T> ExtendedSpecification<T> overlaptLocalDateTime(Range<LocalDateTime> range, Function<From<?, ? extends T>, Expression<Date>> databaseColumnStartRange,
		Function<From<?, ? extends T>, Expression<Date>> databaseColumnEndRange)
	{
		var newRange = DateUtil.toUtilDateRange(range);
		return (r, q, cb) -> maakRangePredicates(cb, newRange, databaseColumnEndRange.apply(r), databaseColumnStartRange.apply(r));
	}

	public static <T> ExtendedSpecification<T> overlaptLocalDateToDate(Range<LocalDate> range, Function<From<?, ? extends T>, Expression<Date>> databaseColumnStartRange,
		Function<From<?, ? extends T>, Expression<Date>> databaseColumnEndRange)
	{
		return overlaptLocalDateTime(DateUtil.toLocalDateTimeRange(range), databaseColumnStartRange, databaseColumnEndRange);
	}

	public static <T> ExtendedSpecification<T> overlaptLocalDate(Range<LocalDate> range, Function<From<?, ? extends T>, Expression<LocalDateTime>> databaseColumnStartRange,
		Function<From<?, ? extends T>, Expression<LocalDateTime>> databaseColumnEndRange)
	{
		return overlapt(DateUtil.toLocalDateTimeRange(range), databaseColumnEndRange, databaseColumnStartRange);
	}

	public static <T> ExtendedSpecification<T> bevatLocalDateTime(Range<LocalDateTime> range, Function<From<?, ? extends T>, Expression<Date>> databaseColumn)
	{
		return bevat(DateUtil.toUtilDateRange(range), databaseColumn);
	}

	public static <T> ExtendedSpecification<T> bevatLocalDateToDate(Range<LocalDate> range, Function<From<?, ? extends T>, Expression<Date>> databaseColumn)
	{
		return bevatLocalDateTime(DateUtil.toLocalDateTimeRange(range), databaseColumn);
	}

	public static <T> ExtendedSpecification<T> bevatLocalDate(Range<LocalDate> range, Function<From<?, ? extends T>, Expression<LocalDateTime>> databaseColumn)
	{
		return overlapt(DateUtil.toLocalDateTimeRange(range), databaseColumn, databaseColumn);
	}

}
