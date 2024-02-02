package nl.rivm.screenit.util.functionalinterfaces;

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

import java.util.function.Function;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.SingularAttribute;

import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

@FunctionalInterface
public interface PathAwarePredicate<T>
{
	Predicate withPath(CriteriaBuilder cb, Path<T> r);

	default Specification<T> toSpecification()
	{
		return (r, q, cb) -> withPath(cb, r);
	}

	default <S> Specification<S> toSpecification(Function<Root<S>, Path<T>> pathSupplier)
	{
		return (r, q, cb) -> withPath(cb, pathSupplier.apply(r));
	}

	default <S> Specification<S> toSpecification(SingularAttribute<? super S, T> attribute)
	{
		return toSpecification(attribute, JoinType.INNER);
	}

	default <S> Specification<S> toSpecification(SingularAttribute<? super S, T> attribute, JoinType joinType)
	{
		return (r, q, cb) -> withPath(cb, SpecificationUtil.join(r, attribute, joinType));
	}
}
