package nl.rivm.screenit.batch.jobs.mamma.kansberekening;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.lang.reflect.ParameterizedType;
import java.util.List;
import java.util.function.Consumer;

import javax.persistence.EntityGraph;
import javax.persistence.FlushModeType;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.repository.impl.FluentJpaQueryImpl;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftIdIn;

@Slf4j
public abstract class MammaAbstractEventWriter<T extends HibernateObject> implements ItemWriter<Long>
{
	private ExecutionContext executionContext;

	@Autowired
	private HibernateService hibernateService;

	@BeforeStep
	public void init(StepExecution stepExecution)
	{
		executionContext = stepExecution.getJobExecution().getExecutionContext();
	}

	protected void aantalContextOphogen(String key)
	{
		if (!executionContext.containsKey(key))
		{
			executionContext.putLong(key, 1L);
		}
		else
		{
			var aantal = executionContext.getLong(key) + 1L;
			executionContext.putLong(key, aantal);

			if (aantal % 10000 == 0)
			{
				LOG.info("Aantal geschreven {}", aantal);
			}
		}
	}

	@Override
	public void write(List<? extends Long> items)
	{
		var session = hibernateService.getHibernateSession();
		session.setFlushMode(FlushModeType.COMMIT);
		@SuppressWarnings("unchecked")
		var ids = (List<Long>) items;
		var entityClass = getEntityClass(getClass());

		var jpaQuery = new FluentJpaQueryImpl<>(heeftIdIn(ids), session, entityClass, entityClass);

		jpaQuery.fetch(getEntityGraphFunction())
			.all()
			.forEach(this::write);
	}

	protected abstract void write(T item);

	protected abstract Consumer<EntityGraph<T>> getEntityGraphFunction();

	@SuppressWarnings("unchecked")
	private Class<T> getEntityClass(Class<?> clazz)
	{
		if (clazz.getGenericSuperclass() instanceof ParameterizedType)
		{
			return (Class<T>) ((ParameterizedType) clazz.getGenericSuperclass()).getActualTypeArguments()[0];
		}
		else
		{
			return getEntityClass(clazz.getSuperclass());
		}
	}
}
