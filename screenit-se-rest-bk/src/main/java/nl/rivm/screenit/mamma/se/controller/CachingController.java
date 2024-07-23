package nl.rivm.screenit.mamma.se.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDateTime;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

abstract class CachingController<TCache extends List> extends AuthorizedController
{
	private static final Logger LOG = LoggerFactory.getLogger(CachingController.class);

	private static final int MAX_CACHE_AGE_MINUTES = 1;

	private final Object cacheLock = new Object();

	private TCache cache;

	private LocalDateTime lastCachedMoment = null;

	protected TCache getCacheContents()
	{
		return cache;
	}

	protected void refreshCacheWhenNeeded()
	{
		if (needCacheRefresh())
		{
			synchronized (cacheLock)
			{
				if (needCacheRefresh())
				{
					cache = getDataToPutInCache();
					LOG.info("Cache refreshed for {} (#elements {})", getClass().getSimpleName(), cache.size());
					lastCachedMoment = LocalDateTime.now();
				}
			}
		}
	}

	protected void resetCache()
	{
		lastCachedMoment = null;
	}

	abstract protected TCache getDataToPutInCache();

	private boolean needCacheRefresh()
	{
		return lastCachedMoment == null || lastCachedMoment.isBefore(LocalDateTime.now().minusMinutes(MAX_CACHE_AGE_MINUTES)) || cache.isEmpty();
	}
}
