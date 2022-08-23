package nl.rivm.screenit.batch.jobs.mamma.kansberekening;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Criteria;
import org.hibernate.FlushMode;
import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

@Slf4j
public abstract class MammaAbstractEventWriter<T> implements ItemWriter<Long>
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
				LOG.info("Aantal geschreven " + aantal);
			}
		}
	}

	@Override
	public void write(List<? extends Long> ids)
	{
		var session = hibernateService.getHibernateSession();
		session.setFlushMode(FlushMode.COMMIT);
		session.enableFetchProfile("kansberekening");

		var criteria = getCriteria(session);
		criteria.add(Restrictions.in("id", ids));

		for (T item : (List<T>) criteria.list())
		{
			write(item);
		}
	}

	protected abstract void write(T item);

	protected abstract Criteria getCriteria(Session session);

}
