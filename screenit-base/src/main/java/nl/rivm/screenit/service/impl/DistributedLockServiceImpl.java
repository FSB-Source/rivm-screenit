package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.service.DistributedLockService;

import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.CuratorFrameworkFactory;
import org.apache.curator.framework.recipes.locks.InterProcessSemaphoreMutex;
import org.apache.curator.retry.ExponentialBackoffRetry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.util.StopWatch;

@Service
public class DistributedLockServiceImpl implements DistributedLockService
{
	private static final Logger LOG = LoggerFactory.getLogger(DistributedLockServiceImpl.class);

	@Autowired
	private String zooKeeperServerUri;

	@Autowired
	@Qualifier("applicationEnvironment")
	private String applicationEnvironment;

	private Map<DistributedLockKey, InterProcessSemaphoreMutex> locks = new HashMap<>();

	private CuratorFramework curatorFramework = null;

	@Override
	public void lockAndWait(String locknaam)
	{
		lockAndWait(locknaam, null);
	}

	@Override
	public void lockAndWait(String locknaam, InstellingGebruiker gebruiker)
	{
		DistributedLockKey lockKey = new DistributedLockKey(createLocknaamVoorOmgeving(locknaam), gebruiker);
		LOG.debug("Try to lock: '" + lockKey.toString() + "'");
		StopWatch stopWatch = startNewStopwatch();

		lock(lockKey);

		LOG.debug("Lock on '" + lockKey.toString() + "' acquired in " + eindtijdStopwatch(stopWatch) + " ms");
	}

	private synchronized void lock(DistributedLockKey lockKey)
	{
		CuratorFramework zooKeeper = getZooKeeperClient();
		InterProcessSemaphoreMutex lock = new InterProcessSemaphoreMutex(zooKeeper, "/" + lockKey.getLocknaam());

		try
		{
			lock.acquire();
			locks.put(lockKey, lock);
		}
		catch (Exception e)
		{
			throw new RuntimeException("Onbekende fout bij locken van " + lockKey.toString(), e);
		}
	}

	@Override
	public synchronized void unlock(String locknaam)
	{
		unlock(locknaam, null);
	}

	@Override
	public void unlock(String locknaam, InstellingGebruiker gebruiker)
	{
		unlock(new DistributedLockKey(createLocknaamVoorOmgeving(locknaam), gebruiker));
	}

	private void unlock(DistributedLockKey lockKey)
	{
		LOG.debug("Try to unlock: " + lockKey);

		InterProcessSemaphoreMutex lock = locks.get(lockKey);
		if (lock != null)
		{
			try
			{
				if (lock.isAcquiredInThisProcess())
				{
					StopWatch stopWatch = startNewStopwatch();
					lock.release();
					LOG.debug("Unlocked '" + lockKey.toString() + "' in " + eindtijdStopwatch(stopWatch) + " ms");
				}
			}
			catch (Exception e)
			{
				LOG.error("Onbekende fout bij unlocken", e);
			}
			finally
			{
				locks.remove(lockKey);
			}
		}
	}

	private CuratorFramework getZooKeeperClient()
	{
		if (curatorFramework == null)
		{
			curatorFramework = CuratorFrameworkFactory.newClient(zooKeeperServerUri, new ExponentialBackoffRetry(1000, 3));
			curatorFramework.start();
		}
		return curatorFramework;
	}

	private String createLocknaamVoorOmgeving(String locknaam)
	{
		return applicationEnvironment + '/' + locknaam;
	}

	private StopWatch startNewStopwatch()
	{
		StopWatch stopWatch = new StopWatch();
		stopWatch.start();
		return stopWatch;
	}

	private long eindtijdStopwatch(StopWatch stopWatch)
	{
		stopWatch.stop();
		return stopWatch.getLastTaskTimeMillis();
	}
}
