package nl.rivm.screenit.repository.impl;

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

import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;

import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.rivm.screenit.specification.SQueryBuilder;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.hibernate.proxy.HibernateProxy;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.support.JpaEntityInformation;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;
import org.springframework.transaction.annotation.Transactional;

public class BaseJpaRepositoryImpl<T extends HibernateObject> extends SimpleJpaRepository<T, Long> implements BaseJpaRepository<T>
{
	public BaseJpaRepositoryImpl(JpaEntityInformation<T, ?> entityInformation, EntityManager entityManager)
	{
		super(entityInformation, entityManager);
	}

	@Override
	public Optional<T> findFirst(Specification<T> specification, Sort sort)
	{
		var typedQuery = getQuery(specification, sort);
		typedQuery.setMaxResults(1);
		return typedQuery.getResultList().stream().findFirst();
	}

	@Override
	public <P> List<P> findAll(Specification<T> specification, Class<P> projectionClass, BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> selectionFunctions)
	{
		return SQueryBuilder.ofProjection(projectionClass, getDomainClass(), selectionFunctions).where(specification).createHQuery().getResultList();
	}

	@Override
	public <P> List<P> findAll(Specification<T> specification, Sort sort, Class<P> projectionClass, BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> selectionFunctions)
	{
		return SQueryBuilder.ofProjection(projectionClass, getDomainClass(), selectionFunctions).where(specification).orderBy(sort).createHQuery().getResultList();
	}

	@Override
	public <P> Optional<P> findFirst(Specification<T> specification, Sort sort, Class<P> projectionClass,
		BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> selectionFunctions)
	{
		return SQueryBuilder.ofProjection(projectionClass, getDomainClass(), selectionFunctions).where(specification).orderBy(sort).createHQuery().setMaxResults(1).getResultList()
			.stream().findFirst();
	}

	@Override
	public <P> Optional<P> findOne(Specification<T> specification, Class<P> projectionClass, BiFunction<CriteriaBuilder, Root<T>, List<Selection<?>>> selectionFunctions)
	{
		return Optional.ofNullable(
			SQueryBuilder.ofProjection(projectionClass, getDomainClass(), selectionFunctions).where(specification).createHQuery().setMaxResults(2).uniqueResult());
	}

	@Override
	@Transactional
	public <S extends T> @NotNull S save(@NotNull S entity)
	{
		var savedEntity = super.save(entity);
		if (entity instanceof HibernateProxy)
		{

			var hibernateLazyInitializer = ((HibernateProxy) entity).getHibernateLazyInitializer();
			if (hibernateLazyInitializer.getInternalIdentifier() == null)
			{
				var savedId = savedEntity.getId();
				hibernateLazyInitializer.setIdentifier(savedId);
			}
		}
		return savedEntity;
	}
}
