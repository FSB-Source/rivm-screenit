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
import java.util.function.Function;

import javax.persistence.EntityManager;

import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.rivm.screenit.repository.FluentJpaQuery;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.hibernate.proxy.HibernateProxy;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.support.JpaEntityInformation;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;
import org.springframework.data.repository.query.FluentQuery;
import org.springframework.lang.Nullable;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Transactional(propagation = Propagation.SUPPORTS)

public class BaseJpaRepositoryImpl<T extends HibernateObject> extends SimpleJpaRepository<T, Long> implements BaseJpaRepository<T>
{
	private final EntityManager entityManager;

	public BaseJpaRepositoryImpl(JpaEntityInformation<T, ?> entityInformation, EntityManager entityManager)
	{
		super(entityInformation, entityManager);
		this.entityManager = entityManager;
	}

	@Override
	public Optional<T> findFirst(Specification<T> specification, Sort sort)
	{
		var typedQuery = getQuery(specification, sort);
		typedQuery.setMaxResults(1);
		return typedQuery.getResultList().stream().findFirst();
	}

	@Override
	public <R> R findWith(Specification<T> specification, Function<FluentJpaQuery<T, T>, R> queryFunction)
	{
		return findWith(specification, getDomainClass(), queryFunction);
	}

	@Override
	public <R, P> R findWith(Specification<T> specification, Class<P> projectionType, Function<FluentJpaQuery<T, P>, R> queryFunction)
	{
		var fluentQuery = new FluentJpaQueryImpl<>(specification, this.entityManager, getDomainClass(), projectionType);
		return queryFunction.apply(fluentQuery);
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

	@Override
	public Optional<T> findById(Long id)
	{
		return super.findById(id);
	}

	@Override
	public T getReferenceById(Long id)
	{
		return super.getReferenceById(id);
	}

	@Override
	public boolean existsById(Long id)
	{
		return super.existsById(id);
	}

	@Override
	public List<T> findAll()
	{
		return super.findAll();
	}

	@Override
	public List<T> findAllById(Iterable<Long> ids)
	{
		return super.findAllById(ids);
	}

	@Override
	public List<T> findAll(Sort sort)
	{
		return super.findAll(sort);
	}

	@Override
	public Page<T> findAll(Pageable pageable)
	{
		return super.findAll(pageable);
	}

	@Override
	public Optional<T> findOne(@Nullable Specification<T> spec)
	{
		return super.findOne(spec);
	}

	@Override
	public List<T> findAll(@Nullable Specification<T> spec)
	{
		return super.findAll(spec);
	}

	@Override
	public Page<T> findAll(@Nullable Specification<T> spec, Pageable pageable)
	{
		return super.findAll(spec, pageable);
	}

	@Override
	public List<T> findAll(@Nullable Specification<T> spec, Sort sort)
	{
		return super.findAll(spec, sort);
	}

	@Override
	public <S extends T> Optional<S> findOne(Example<S> example)
	{
		return super.findOne(example);
	}

	@Override
	public <S extends T> long count(Example<S> example)
	{
		return super.count(example);
	}

	@Override
	public <S extends T> boolean exists(Example<S> example)
	{
		return super.exists(example);
	}

	@Override
	public boolean exists(Specification<T> spec)
	{
		return super.exists(spec);
	}

	@Override
	public <S extends T> List<S> findAll(Example<S> example)
	{
		return super.findAll(example);
	}

	@Override
	public <S extends T> List<S> findAll(Example<S> example, Sort sort)
	{
		return super.findAll(example, sort);
	}

	@Override
	public <S extends T> Page<S> findAll(Example<S> example, Pageable pageable)
	{
		return super.findAll(example, pageable);
	}

	@Override
	public <S extends T, R> R findBy(Example<S> example, Function<FluentQuery.FetchableFluentQuery<S>, R> queryFunction)
	{
		return super.findBy(example, queryFunction);
	}

	@Override
	public long count()
	{
		return super.count();
	}

	@Override
	public long count(@Nullable Specification<T> spec)
	{
		return super.count(spec);
	}
}
