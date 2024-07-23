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
import lombok.AllArgsConstructor;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class SpecificationUtil
{
	public static Predicate containsCaseInsensitive(CriteriaBuilder cb, Path<String> path, String keyword)
	{
		return cb.like(
			cb.lower(path),
			cb.lower(cb.literal("%" + escapeLikeString(keyword) + "%"))
		);
	}

	public static Predicate containsCaseSensitive(CriteriaBuilder cb, Path<String> path, String keyword)
	{
		return cb.like(
			path,
			cb.literal("%" + escapeLikeString(keyword) + "%")
		);
	}

	public static <S> Specification<S> skipWhenEmpty(String keyword, Specification<S> specification)
	{
		return (r, q, cb) -> StringUtils.isBlank(keyword) ? null : specification.toPredicate(r, q, cb);
	}

	public static <S> Specification<S> skipWhenEmpty(List<?> list, Specification<S> specification)
	{
		return (r, q, cb) -> list.isEmpty() ? null : specification.toPredicate(r, q, cb);
	}

	public static <S> Specification<S> skipWhenNull(Object object, Specification<S> specification)
	{
		return (r, q, cb) -> object == null ? null : specification.toPredicate(r, q, cb);
	}

	public static <S> Specification<S> skipWhenNotTrue(Boolean value, Specification<S> specification)
	{
		return (r, q, cb) -> Boolean.TRUE.equals(value) ? specification.toPredicate(r, q, cb) : null;
	}

	public static <S> Specification<S> skipWhenFalsy(boolean value, Specification<S> specification)
	{
		return (r, q, cb) -> !value ? null : specification.toPredicate(r, q, cb);
	}

	public static Predicate skipWhenBlankPredicate(String keyword, Predicate predicate)
	{
		return StringUtils.isBlank(keyword) ? null : predicate;
	}

	public static Predicate skipWhenNullPredicate(Object object, Predicate predicate)
	{
		return object == null ? null : predicate;
	}

	public static <X, Y> Join<X, Y> join(Root<X> root, SingularAttribute<? super X, Y> attribute)
	{
		return join(root, attribute, JoinType.INNER);
	}

	@SuppressWarnings("unchecked")
	public static <X, Y> Join<X, Y> join(Root<X> root, SingularAttribute<? super X, Y> attribute, JoinType joinType)
	{
		return (Join<X, Y>) root
			.getJoins()
			.stream()
			.filter(join -> join.getAttribute().getName().equals(attribute.getName()) && join.getJoinType() == joinType)
			.findFirst()
			.orElseGet(() -> root.join(attribute, joinType));
	}

	public static <X, Y> Join<X, Y> join(Root<X> root, ListAttribute<? super X, Y> attribute)
	{
		return join(root, attribute, JoinType.INNER);
	}

	@SuppressWarnings("unchecked")
	public static <X, Y> Join<X, Y> join(Root<X> root, ListAttribute<? super X, Y> attribute, JoinType joinType)
	{
		return (Join<X, Y>) root
			.getJoins()
			.stream()
			.filter(join -> join.getAttribute().getName().equals(attribute.getName()) && join.getJoinType() == joinType)
			.findFirst()
			.orElseGet(() -> root.join(attribute, joinType));
	}

	public static <X, Y, Z> Join<X, Y> join(From<Z, X> from, ListAttribute<? super X, Y> attribute)
	{
		return join(from, attribute, JoinType.INNER);
	}

	@SuppressWarnings("unchecked")
	public static <X, Y, Z> Join<X, Y> join(From<Z, X> from, ListAttribute<? super X, Y> attribute, JoinType joinType)
	{
		return (Join<X, Y>) from
			.getJoins()
			.stream()
			.filter(join -> join.getAttribute().getName().equals(attribute.getName()) && join.getJoinType() == joinType)
			.findFirst()
			.orElseGet(() -> from.join(attribute, joinType));
	}

	public static <X, Y, Z> Join<X, Y> join(From<Z, X> from, SingularAttribute<? super X, Y> attribute)
	{
		return join(from, attribute, JoinType.INNER);
	}

	@SuppressWarnings("unchecked")
	public static <X, Y, Z> Join<X, Y> join(From<Z, X> from, SingularAttribute<? super X, Y> attribute, JoinType joinType)
	{
		return (Join<X, Y>) from
			.getJoins()
			.stream()
			.filter(join -> join.getAttribute().getName().equals(attribute.getName()) && join.getJoinType() == joinType)
			.findFirst()
			.orElseGet(() -> from.join(attribute, joinType));
	}

	public static Predicate composePredicates(CriteriaBuilder cb, List<Predicate> predicates)
	{
		return cb.and(predicates.toArray(Predicate[]::new));
	}

	public static Predicate composePredicatesOr(CriteriaBuilder cb, List<Predicate> predicates)
	{
		return cb.or(predicates.toArray(Predicate[]::new));
	}

	private static String escapeLikeString(String s)
	{
		return s != null ? s.replaceAll("([_%])", "\\\\$1") : "";
	}
}
