package nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.Collection;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.batch.service.impl.ColonUitnodigingsgebiedSelectieContext;
import nl.rivm.screenit.model.colon.ClientCategorieEntry;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;

import org.springframework.batch.item.ExecutionContext;

@Slf4j
public class ClientSelectieMetCapaciteitItemCursor implements ClientSelectieItemIterator
{

	private final Collection<ColonUitnodigingsgebiedSelectieContext> alleUitnodigingsgebieden;

	private boolean cursorClosed = false;

	private final Queue<ClientCategorieEntry> concurrentEntries = new ConcurrentLinkedQueue<>();

	private ForkJoinPool forkJoinPool;

	private final ColonClientSelectieContext selectieContext;

	private CountDownLatch latch = null;

	public ClientSelectieMetCapaciteitItemCursor(ColonClientSelectieContext selectieContext, Collection<ColonUitnodigingsgebiedSelectieContext> uitnodigingsgebieden)
	{
		this.selectieContext = selectieContext;
		this.alleUitnodigingsgebieden = uitnodigingsgebieden;

		startSelectieThreads(uitnodigingsgebieden);
	}

	private void startSelectieThreads(Collection<ColonUitnodigingsgebiedSelectieContext> uitnodigingsgebieden)
	{
		int aantalBeschikbareCores = Runtime.getRuntime().availableProcessors();
		int aantalThreadsVoorGenereren = Math.min(Math.min(aantalBeschikbareCores * BatchConstants.AANTAL_THREADS_PER_CORE, BatchConstants.MAXIMUM_AANTAL_THREADS),
			uitnodigingsgebieden.size());
		LOG.info("Gebruik " + aantalThreadsVoorGenereren + " threads voor het selecteren van clienten.");
		forkJoinPool = new ForkJoinPool(aantalThreadsVoorGenereren);
		LOG.info("Start selecteren van clienten voor " + uitnodigingsgebieden.size() + " uitnodigingsgebieden.");
		for (ColonUitnodigingsgebiedSelectieContext uitnodigingsgebied : uitnodigingsgebieden)
		{
			forkJoinPool.submit(() -> OpenHibernate5Session.withCommittedTransaction().run(() -> selecteerClientenVoorUitnodigingsgebied(uitnodigingsgebied)));
		}
		forkJoinPool.shutdown();
	}

	private void selecteerClientenVoorUitnodigingsgebied(ColonUitnodigingsgebiedSelectieContext uitnodigingsgebied)
	{
		String uitnodigingsgebiedNaam = uitnodigingsgebied.getUitnodigingsgebiedNaam();
		LOG.info("Selectie voor uitnodigingsgebied '{}' (id: '{}') is gestart.", uitnodigingsgebiedNaam, uitnodigingsgebied.getUitnodigingsgebiedId());
		ClientSelectieMetCapaciteitPerGebiedItemCursor cursor = null;
		try
		{
			cursor = new ClientSelectieMetCapaciteitPerGebiedItemCursor(selectieContext, uitnodigingsgebied);

			ClientCategorieEntry clientCategorieEntry;
			while (cursor.hasNext() && !cursorClosed)
			{
				clientCategorieEntry = cursor.next();
				if (clientCategorieEntry != null)
				{
					concurrentEntries.add(clientCategorieEntry);
					synchronized (this)
					{
						if (latch != null)
						{
							latch.countDown();
							LOG.trace("Latch countDown");
							latch = null;
						}
					}
				}
			}
		}
		catch (Exception e)
		{
			LOG.error("Fout in selectie voor uitnodigingsgebied '{}'", uitnodigingsgebiedNaam, e);
		}
		finally
		{
			if (cursor != null)
			{
				cursor.close();
			}
			LOG.info("Selectie voor uitnodigingsgebied '{}' is afgerond.", uitnodigingsgebiedNaam);
		}
	}

	@Override
	public boolean hasNext()
	{
		if (cursorClosed)
		{
			return false;
		}
		boolean hasNext = heeftEenThreadEenEntry();
		if (!hasNext && !cursorClosed)
		{
			Collection<ColonUitnodigingsgebiedSelectieContext> aangepasteUitnodigingsgebieden = selectieContext.uitnodigingsGebiedCapaciteitService
				.leegloopHerverdelen(alleUitnodigingsgebieden);
			if (!aangepasteUitnodigingsgebieden.isEmpty())
			{
				startSelectieThreads(aangepasteUitnodigingsgebieden);
				hasNext = heeftEenThreadEenEntry();
			}
		}

		if (LOG.isDebugEnabled())
		{
			LOG.debug("hasNext=" + hasNext);
		}
		return hasNext;
	}

	private boolean heeftEenThreadEenEntry()
	{
		boolean hasNext = !concurrentEntries.isEmpty();
		while (!hasNext && !forkJoinPool.isTerminated() && !cursorClosed)
		{
			try
			{
				synchronized (this)
				{
					if (latch == null)
					{
						LOG.trace("Latch created");
						latch = new CountDownLatch(1);
					}
				}
				boolean await = latch.await(100, TimeUnit.MILLISECONDS);
				LOG.trace("Latch " + (latch != null ? latch + " " + await : " gone"));
			}
			catch (InterruptedException e)
			{
				LOG.error("Fout in wachten op await", e);
			}
			hasNext = !concurrentEntries.isEmpty();
		}
		return hasNext;
	}

	@Override
	public ClientCategorieEntry next()
	{
		if (!cursorClosed)
		{
			return concurrentEntries.poll();
		}
		return null;
	}

	@Override
	public void clear()
	{

	}

	@Override
	public void close()
	{
		cursorClosed = true;
		try
		{
			forkJoinPool.awaitTermination(1, TimeUnit.SECONDS);
		}
		catch (InterruptedException e)
		{
			LOG.error("Fout in wachten op awaitTermination ", e);
		}
	}

	@Override
	public void saveState(ExecutionContext context)
	{

	}

	@Override
	public void remove()
	{
		throw new UnsupportedOperationException("remove is not supported");
	}

}
