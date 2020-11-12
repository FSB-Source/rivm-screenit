package nl.rivm.screenit.mamma.planning.wijzigingen;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.util.Collection;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

import nl.rivm.screenit.mamma.planning.index.PlanningScreeningsEenheidIndex;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsPeriode;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public enum PlanningDoorrekenenManager
{
	;

	private static final Logger LOG = LoggerFactory.getLogger(PlanningDoorrekenenManager.class);

	private static ThreadPoolExecutor executor = new ThreadPoolExecutor(0, 100, 0, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());

	private static CyclicBarrier barrier;

	private static CountDownLatch countDownLatch;

	private static AtomicBoolean exceptionOccured = new AtomicBoolean();

	public static void run()
	{
		LOG.info("run");

		try
		{
			exceptionOccured.set(false);
			PlanningWijzigingen.getClientSet().forEach(client -> PlanningDoorrekenen.run(client));
			PlanningWijzigingen.getPostcodeReeksRegioSet().forEach(postcodeRegio -> PlanningDoorrekenen.run(postcodeRegio));
			PlanningWijzigingen.getPostcodeReeksSet().forEach(postcodeReeks -> PlanningDoorrekenen.run(postcodeReeks));
			PlanningWijzigingen.getTehuisSet().forEach(tehuis -> PlanningDoorrekenen.run(tehuis));
			PlanningWijzigingen.getStandplaatsSet().parallelStream().forEach(standplaats -> PlanningDoorrekenen.run(standplaats));

			Collection<PlanningWijzigingenRoute> wijzigingenRoutes = PlanningWijzigingen.getWijzigingenRoutes();
			if (!wijzigingenRoutes.isEmpty())
			{
				executor.setCorePoolSize(PlanningScreeningsEenheidIndex.size());
				barrier = new CyclicBarrier(wijzigingenRoutes.size());
				countDownLatch = new CountDownLatch(wijzigingenRoutes.size());

				wijzigingenRoutes.forEach(wijzigingenRoute -> run(wijzigingenRoute));

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

				wijzigingenRoute.getBlokSet().forEach(blok -> PlanningDoorrekenenRoute.run(blok));
				wijzigingenRoute.getDagSet().forEach(dag -> PlanningDoorrekenenRoute.run(dag));
				wijzigingenRoute.getWeekSet().forEach(week -> PlanningDoorrekenenRoute.run(week));

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
						.forEach(standplaatsRonde -> PlanningDoorrekenenRoute.run(standplaatsRonde));
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
			screeningsEenheid.getStandplaatsPeriodeNavigableSet().tailSet(vanafStandplaatsPeriode, true).stream().filter(standplaatsPeriode -> standplaatsPeriode.gesplitst())
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
