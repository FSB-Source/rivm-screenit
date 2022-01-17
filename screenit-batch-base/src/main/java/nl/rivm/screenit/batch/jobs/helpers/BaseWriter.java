package nl.rivm.screenit.batch.jobs.helpers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.model.enums.Level;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.HibernateException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class BaseWriter<S extends HibernateObject> implements ItemWriter<Long>
{

	private static final Logger LOG = LoggerFactory.getLogger(BaseWriter.class);

	private JobExecution jobExecution;

	private StepExecution stepExecution;

	@Autowired
	private HibernateService hibernateService;

	public static <E extends Enum> String getEnumKey(String enumKey, E enumConstant)
	{
		return enumKey + enumConstant;
	}

	@Override
	public void write(List<? extends Long> items) throws Exception
	{
		try
		{
			beforeWrite();
			writeItems(items);
		}
		catch (HibernateException e)
		{
			if (e.getMessage().equals("Illegal attempt to associate a collection with two open sessions"))
			{
				LOG.error("Is er een proccessor in je batch job?");
			}
			else
			{
				LOG.error("Een database error is opgetreden bij de aanpassing in de database.");
			}
			crashMelding("De job heeft onsuccesvol gedraaid, neem contact op met de helpdesk.", e);
			throw e;
		}
		catch (IllegalStateException e)
		{
			crashMelding(e.getMessage(), e);
			throw e;
		}
		catch (Exception e)
		{
			crashMelding("De job heeft onsuccesvol gedraaid, neem contact op met de helpdesk.", e);
			throw e;
		}
		finally
		{
			afterWrite();
		}
	}

	public void writeItems(List<? extends Long> items) throws Exception
	{
		Class<S> entityClass = getEntityClass(getClass());
		for (Long item : items)
		{
			if (HibernateObject.class.isAssignableFrom(entityClass))
			{
				S entity = hibernateService.get(entityClass, item);
				if (entity != null)
				{
					write(entity);
				}
				else
				{
					LOG.error("In writer " + this.getClass().getName() + " wordt een entity geladen die niet meer beschrikbaar is " + entityClass.getName() + "." + item);
				}
			}
		}
	}

	@SuppressWarnings("unchecked")
	private Class<S> getEntityClass(Class<?> clazz)
	{
		if (clazz.getGenericSuperclass() instanceof ParameterizedType)
		{
			return (Class<S>) ((ParameterizedType) clazz.getGenericSuperclass()).getActualTypeArguments()[0];
		}
		else
		{
			return getEntityClass(clazz.getSuperclass());
		}
	}

	protected void beforeWrite() throws Exception
	{

	}

	protected void afterWrite()
	{

	}

	protected abstract void write(S item) throws Exception;

	protected ExecutionContext getExecutionContext()
	{
		return jobExecution.getExecutionContext();

	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
		this.jobExecution = stepExecution.getJobExecution();
	}

	protected void crashMelding(String melding, Exception e)
	{
		LOG.error(melding, e);
		if (!getExecutionContext().containsKey(BatchConstants.MELDING) || !Level.ERROR.equals(getExecutionContext().get(BatchConstants.LEVEL)))
		{
			getExecutionContext().put(BatchConstants.MELDING, melding);
			getExecutionContext().put(BatchConstants.LEVEL, Level.ERROR);
		}
	}

	protected final HibernateService getHibernateService()
	{
		return hibernateService;
	}

	public void setHibernateService(HibernateService hibernateService)
	{
		this.hibernateService = hibernateService;
	}

	public JobExecution getJobExecution()
	{
		return jobExecution;
	}

	public StepExecution getStepExecution()
	{
		return stepExecution;
	}

	protected void aantalContextOphogen(String key)
	{
		ExecutionContext context = getExecutionContext();
		if (!context.containsKey(key))
		{
			context.putLong(key, 1L);
		}
		else
		{
			context.putLong(key, context.getLong(key) + 1L);
		}
	}

	protected <E extends Enum> void aantalContextOphogen(String enumKey, E enumConstant)
	{
		aantalContextOphogen(getEnumKey(enumKey, enumConstant));
	}

	protected <E extends Enum> void aantallenContextOphogen(String key, String enumKey, E enumConstant)
	{
		aantalContextOphogen(key);
		aantalContextOphogen(enumKey, enumConstant);
	}
}
