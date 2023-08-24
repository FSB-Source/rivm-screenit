package nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronizationManager;

@Component
@StepScope
public class ClientSelectieItemReader extends AbstractClientSelectieReader
{
	private final ICurrentDateSupplier currentDateSupplier;

	public ClientSelectieItemReader(ICurrentDateSupplier currentDateSupplier)
	{
		super.setFetchSize(50);
		this.currentDateSupplier = currentDateSupplier;
	}

	@Override
	public void open(ExecutionContext executionContext) throws ItemStreamException
	{
		boolean unbindSessionFromThread = false;
		try
		{
			hibernateSession = sessionFactory.openSession();
			if (!TransactionSynchronizationManager.hasResource(sessionFactory))
			{
				TransactionSynchronizationManager.bindResource(sessionFactory, new SessionHolder(hibernateSession));
				unbindSessionFromThread = true;
			}
			context = executionContext;
			cursor = new ClientSelectieItemCursor(hibernateSession, fetchSize, context, uitgenodigdeClientIds, fitService, currentDateSupplier.getLocalDate());

		}
		finally
		{
			if (unbindSessionFromThread)
			{
				TransactionSynchronizationManager.unbindResource(sessionFactory);
			}
		}
	}

	@Override
	public ClientCategorieEntry read()
	{
		return getNextEntry();
	}
}
