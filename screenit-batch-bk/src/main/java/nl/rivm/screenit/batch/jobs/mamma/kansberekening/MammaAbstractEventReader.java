package nl.rivm.screenit.batch.jobs.mamma.kansberekening;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Iterator;
import nl.rivm.screenit.datasource.DataSourceRouter;
import org.hibernate.Criteria;
import org.hibernate.SessionFactory;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Projections;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStream;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class MammaAbstractEventReader implements ItemReader<Long>, ItemStream
{
	@Autowired
	private SessionFactory sessionFactory;

	private ThreadLocal<Iterator<Long>> threadLocalIterator = new ThreadLocal<>();

	@Override
	public void open(ExecutionContext executionContext)
	{
		DataSourceRouter.useReadOnly();
		StatelessSession session = sessionFactory.openStatelessSession();

		Criteria criteria = getCriteria(session);
		criteria.setProjection(Projections.id());

		threadLocalIterator.set(criteria.list().iterator());

		session.close();
		DataSourceRouter.useReadWrite();
	}

	@Override
	public Long read()
	{
		Iterator<Long> iterator = threadLocalIterator.get();
		if (iterator.hasNext())
		{
			return iterator.next();
		}
		return null;
	}

	@Override
	public void close()
	{
	}

	@Override
	public void update(ExecutionContext executionContext)
	{
	}

	protected abstract Criteria getCriteria(StatelessSession session);
}
