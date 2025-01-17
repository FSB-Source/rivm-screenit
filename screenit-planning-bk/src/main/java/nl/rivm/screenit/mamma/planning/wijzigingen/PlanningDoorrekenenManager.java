package nl.rivm.screenit.mamma.planning.wijzigingen;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;

@Slf4j
public enum PlanningDoorrekenenManager
{
	;

	private static final ThreadPoolExecutor executor = new ThreadPoolExecutor(0, 100, 0, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());

	private static final AtomicBoolean exceptionOccured = new AtomicBoolean();

	private static CyclicBarrier barrier;

	private static CountDownLatch countDownLatch;

	public static void run()
	{
		LOG.info("run");

		try
		{
			exceptionOccured.set(false);
			PlanningWijzigingen.getClientSet().forEach(PlanningDoorrekenen::run);
			PlanningWijzigingen.getPostcodeReeksRegioSet().forEach(PlanningDoorrekenen::run);
			PlanningWijzigingen.getPostcodeReeksSet().forEach(PlanningDoorrekenen::run);
			PlanningWijzigingen.getTehuisSet().forEach(PlanningDoorrekenen::run);
			PlanningWijzigingen.getStandplaatsSet().parallelStream().forEach(PlanningDoorrekenen::run);

			Collection<PlanningWijzigingenRoute> wijzigingenRoutes = PlanningWijzigingen.getWijzigingenRoutes();
			if (!wijzigingenRoutes.isEmpty())
			{
				executor.setCorePoolSize(PlanningScreeningsEenheidIndex.size());
				barrier = new CyclicBarrier(wijzigingenRoutes.size());
				countDownLatch = new CountDownLatch(wijzigingenRoutes.size());

				wijzigingenRoutes.forEach(PlanningDoorrekenenManager::run);

				if (!countDownLatch.await(10, TimeUnit.SECONDS))
				{
					throw new TimeoutException();
				}
			}
		}
		catch (InterruptedException | TimeoutException e)
		{
			throw new RuntimeException(e);
		}
		finally
		{
			PlanningWijzigingen.clear();

			if (exceptionOccured.get())
			{
				throw (new RuntimeException("Exceptie in thread"));
			}
		}

	}

	private static void run(PlanningWijzigingenRoute wijzigingenRoute)
	{
		LOG.debug("run screeningsEenheid: " + wijzigingenRoute.getScreeningsEenheid().getId());

		executor.submit(() ->
		{
			try
			{
				lockingGesplitsteStandplaatsPeriodes(wijzigingenRoute, true);

				barrier.await(10, TimeUnit.SECONDS);

				wijzigingenRoute.getBlokSet().forEach(PlanningDoorrekenenRoute::run);
				wijzigingenRoute.getDagSet().forEach(PlanningDoorrekenenRoute::run);
				wijzigingenRoute.getWeekSet().forEach(PlanningDoorrekenenRoute::run);

				PlanningStandplaatsPeriode vanafStandplaatsPeriode = wijzigingenRoute.getVanafStandplaatsPeriode();
				if (vanafStandplaatsPeriode != null)
				{
					PlanningDoorrekenenRoute.run(vanafStandplaatsPeriode);
				}

				barrier.await(10, TimeUnit.SECONDS);

				PlanningScreeningsEenheid screeningsEenheid = wijzigingenRoute.getScreeningsEenheid();
				if (vanafStandplaatsPeriode != null)
				{
					screeningsEenheid.getStandplaatsPeriodeNavigableSet().tailSet(vanafStandplaatsPeriode, true).stream()
						.filter(standplaatsPeriode -> !standplaatsPeriode.gesplitst())
						.map(PlanningStandplaatsPeriode::getStandplaatsRonde)
						.forEach(PlanningDoorrekenenRoute::run);
				}

				barrier.await(10, TimeUnit.SECONDS);

				if (vanafStandplaatsPeriode != null)
				{
					PlanningDoorrekenenRoute.run(screeningsEenheid);
				}
			}

			catch (Exception e)
			{
				LOG.error("Exceptie in thread", e);
				exceptionOccured.set(true);
				lockingGesplitsteStandplaatsPeriodes(wijzigingenRoute, false);
			}
			finally
			{
				countDownLatch.countDown();
			}
		});
	}

	private static void lockingGesplitsteStandplaatsPeriodes(PlanningWijzigingenRoute wijzigingenRoute, boolean lock)
	{
		if (wijzigingenRoute.getVanafStandplaatsPeriode() != null)
		{
			PlanningStandplaatsPeriode vanafStandplaatsPeriode = wijzigingenRoute.getVanafStandplaatsPeriode();
			PlanningScreeningsEenheid screeningsEenheid = vanafStandplaatsPeriode.getScreeningsEenheid();
			screeningsEenheid.getStandplaatsPeriodeNavigableSet().tailSet(vanafStandplaatsPeriode, true).stream().filter(PlanningStandplaatsPeriode::gesplitst)
				.forEach(standplaatsPeriode ->
				{
					if (lock)
					{
						standplaatsPeriode.lock();
					}
					else
					{

						try
						{
							standplaatsPeriode.unlock();
						}
						catch (IllegalMonitorStateException e)
						{

						}
					}
				});
		}
	}
}
