package nl.rivm.screenit.batch.jobs.helpers;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.HashMap;
import java.util.Map;

import org.hibernate.SessionFactory;
import org.hibernate.StatelessSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.partition.support.Partitioner;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class BasePartitioner implements Partitioner
{

	@Autowired
	private SessionFactory sessionFactory;

	private StatelessSession hibernateSession;

	@Override
	public Map<String, ExecutionContext> partition(int gridSize)
	{
		Map<String, ExecutionContext> partities = new HashMap<String, ExecutionContext>(gridSize);
		try
		{
			hibernateSession = sessionFactory.openStatelessSession();

			partities = setPartition(gridSize);
		}
		finally
		{
			if (hibernateSession != null)
			{
				hibernateSession.close();
			}
		}
		return partities;
	}

	public abstract Map<String, ExecutionContext> setPartition(int gridSize);

	protected StatelessSession getHibernateSession()
	{
		return hibernateSession;
	}

}
