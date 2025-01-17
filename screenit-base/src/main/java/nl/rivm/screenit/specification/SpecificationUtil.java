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

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.ListAttribute;
import javax.persistence.metamodel.SingularAttribute;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import org.apache.commons.lang.StringUtils;
import org.apache.shiro.util.CollectionUtils;
import org.springframework.data.jpa.domain.Specification;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class SpecificationUtil
{
	public static Predicate containsCaseInsensitive(CriteriaBuilder cb, Path<String> path, String keyword)
	{
		return cb.like(cb.lower(path), "%" + escapeLikeString(keyword).toLowerCase() + "%");
	}

	public static Predicate startsWithCaseInsensitive(CriteriaBuilder cb, Path<String> path, String keyword)
	{
		return cb.like(cb.lower(path), escapeLikeString(keyword).toLowerCase() + "%");
	}

	public static Predicate endsWithCaseInsensitive(CriteriaBuilder cb, Path<String> path, String keyword)
	{
		return cb.like(cb.lower(path), "%" + escapeLikeString(keyword).toLowerCase());
	}

	public static Predicate exactCaseInsensitive(CriteriaBuilder cb, Path<String> path, String keyword)
	{
		return cb.like(cb.lower(path), escapeLikeString(keyword).toLowerCase());
	}

	public static Predicate containsCaseSensitive(CriteriaBuilder cb, Path<String> path, String keyword)
	{
		return cb.like(path, "%" + escapeLikeString(keyword) + "%");
	}

	public static <S> Specification<S> skipWhenEmpty(String keyword, Specification<S> specification)
	{
		return (r, q, cb) -> StringUtils.isBlank(keyword) ? null : specification.toPredicate(r, q, cb);
	}

	public static <S> ExtendedSpecification<S> skipWhenEmptyExtended(String keyword, ExtendedSpecification<S> specification)
	{
		return (r, q, cb) -> StringUtils.isBlank(keyword) ? null : specification.toPredicate(r, q, cb);
	}

	public static <S> ExtendedSpecification<S> skipWhenEmpty(Collection<?> list, ExtendedSpecification<S> specification)
	{
		return (r, q, cb) -> CollectionUtils.isEmpty(list) ? null : specification.toPredicate(r, q, cb);
	}

	public static <S> ExtendedSpecification<S> skipWhenEmptyExtended(Collection<?> list, ExtendedSpecification<S> specification)
	{
		return (r, q, cb) -> CollectionUtils.isEmpty(list) ? null : specification.toPredicate(r, q, cb);
	}

	public static <S> Specification<S> skipWhenNull(Object object, Specification<S> specification)
	{
		return (r, q, cb) -> object == null ? null : specification.toPredicate(r, q, cb);
	}

	public static <S> ExtendedSpecification<S> skipWhenNullExtended(Object object, ExtendedSpecification<S> specification)
	{
		return (r, q, cb) -> object == null ? null : specification.toPredicate(r, q, cb);
	}

	public static <S> Specification<S> skipWhenNotTrue(Boolean value, Specification<S> specification)
	{
		return (r, q, cb) -> Boolean.TRUE.equals(value) ? specification.toPredicate(r, q, cb) : null;
	}

	public static <S> Specification<S> skipWhenFalse(boolean value, Specification<S> specification)
	{
		return (r, q, cb) -> !value ? null : specification.toPredicate(r, q, cb);
	}

	public static Predicate skipWhenBlankPredicate(String keyword, Predicate predicate)
	{
		return StringUtils.isBlank(keyword) ? null : predicate;
	}

	public static <X, Y, Z> Join<X, Y> join(From<Z, X> from, ListAttribute<? super X, Y> attribute)
	{
		return join(from, attribute, JoinType.INNER);
	}

	public static <X, Y> Join<X, Y> join(From<?, X> from, ListAttribute<? super X, Y> attribute, JoinType joinType)
	{
		return addJoinIfNotExists(from, attribute.getName(), joinType, () -> from.join(attribute, joinType));
	}

	public static <X, Y, Z> Join<X, Y> join(From<Z, X> from, SingularAttribute<? super X, Y> attribute)
	{
		return join(from, attribute, JoinType.INNER);
	}

	public static <X, Y> Join<X, Y> join(From<?, X> from, SingularAttribute<? super X, Y> attribute, JoinType joinType)
	{
		return addJoinIfNotExists(from, attribute.getName(), joinType, () -> from.join(attribute, joinType));
	}

	public static <X, Y> Join<X, Y> joinByString(From<?, X> from, String attributeName, JoinType joinType)
	{
		return addJoinIfNotExists(from, attributeName, joinType, () -> from.join(attributeName, joinType));
	}

	@SuppressWarnings("unchecked")
	private static <X, Y> Join<X, Y> addJoinIfNotExists(From<?, X> from, String attributeName, JoinType joinType, Supplier<Join<X, ?>> createJoinFunction)
	{
		return (Join<X, Y>) from
			.getJoins()
			.stream()
			.filter(join -> isZelfdeJoin(join, attributeName, joinType))
			.findFirst()
			.orElseGet(createJoinFunction);
	}

	private static <X> boolean isZelfdeJoin(Join<X, ?> join, String attributeName, JoinType joinType)
	{
		return join.getAttribute().getName().equals(attributeName) && join.getJoinType() == joinType;
	}

	@SuppressWarnings("unchecked")
	public static <X, T extends X> From<?, T> treat(From<?, ? extends X> from, Class<T> type, CriteriaBuilder cb)
	{
		if (from instanceof Root<?>)
		{
			var root = (Root<X>) from;
			return cb.treat(root, type);
		}
		else if (from instanceof Join<?, ?>)
		{
			var join = (Join<?, X>) from;
			return cb.treat(join, type);
		}
		throw new IllegalStateException("Unsupported from type: " + from.getClass());
	}

	public static Predicate composePredicates(CriteriaBuilder cb, List<Predicate> predicates)
	{
		return predicates.stream().filter(Objects::nonNull).reduce(cb::and).orElse(null);
	}

	public static Predicate composePredicates(CriteriaBuilder cb, Predicate... predicates)
	{
		return composePredicates(cb, Arrays.asList(predicates));
	}

	public static Predicate composePredicatesOr(CriteriaBuilder cb, List<Predicate> predicates)
	{
		return predicates.stream().filter(Objects::nonNull).reduce(cb::or).orElse(null);
	}

	private static String escapeLikeString(String s)
	{
		return s != null ? s.replaceAll("([_%])", "\\\\$1") : "";
	}

	public static <X, Y> ExtendedSpecification<X> isAttribuutGelijkOfNull(SingularAttribute<X, Y> attribuut, Object value)
	{
		return (r, q, cb) ->
		{
			if (value != null && StringUtils.isNotBlank(value.toString()))
			{
				return cb.equal(r.get(attribuut), value);
			}
			else
			{
				return cb.isNull(r.get(attribuut));
			}
		};
	}
}
