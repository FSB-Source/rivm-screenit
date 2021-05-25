package nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.dao.ClientDao;
import nl.rivm.screenit.datasource.DataSourceRouter;
import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStream;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.SessionFactoryUtils;

public abstract class AbstractClientSelectieReader implements ItemReader<ClientCategorieEntry>, ItemStream
{

	protected Session hibernateSession;

	protected ClientSelectieItemIterator cursor;

	protected int fetchSize;

	protected StepExecution stepExecution;

	protected ExecutionContext context;

	protected final List<Long> uitgenodigdeClientIds = Collections.synchronizedList(new ArrayList<>());

	@Autowired
	protected SessionFactory sessionFactory;

	@Autowired
	protected SimplePreferenceService preferenceService;

	@Autowired
	protected ClientDao clientDao;

	public void setFetchSize(int fetchSize)
	{
		this.fetchSize = fetchSize;
	}

	@Override
	public void update(ExecutionContext executionContext) throws ItemStreamException
	{
		cursor.saveState(executionContext);
		cursor.clear();
	}

	@Override
	public void close() throws ItemStreamException
	{
		if (cursor != null)
		{
			cursor.close();
		}
		SessionFactoryUtils.closeSession(hibernateSession);
	}

	protected final ClientCategorieEntry getNextEntry()
	{
		ClientCategorieEntry clientCategorieEntry = null;
		try
		{
			while (clientCategorieEntry == null && cursor.hasNext())
			{
				clientCategorieEntry = cursor.next();
			}
		}
		finally
		{
			DataSourceRouter.useReadWrite();
		}
		return clientCategorieEntry;
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
	}
}
