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

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.NoRepositoryBean;

@NoRepositoryBean
public interface BaseJpaRepository<T> extends JpaRepository<T, Long>, JpaSpecificationExecutor<T>
{
	Optional<T> findFirst(Specification<T> specification, Sort sort);

	<P> List<P> findAll(Specification<T> specification, Class<P> projectionClass, BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> selectionFunction);

	<P> List<P> findAll(Specification<T> specification, Sort sort, Class<P> projectionClass, BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> selectionFunction);

	<P> Optional<P> findFirst(Specification<T> specification, Sort sort, Class<P> projectionClass, BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> selectionFunction);

	<P> Optional<P> findOne(Specification<T> specification, Class<P> projectionClass, BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> selectionFunction);
}
