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

import java.util.Optional;

import javax.persistence.EntityManager;

import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.support.JpaEntityInformation;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;

public class BaseJpaRepositoryImpl<T> extends SimpleJpaRepository<T, Long> implements BaseJpaRepository<T>
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
}
