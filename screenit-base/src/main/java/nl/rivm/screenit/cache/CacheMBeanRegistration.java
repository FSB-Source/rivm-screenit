package nl.rivm.screenit.cache;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.lang.management.ManagementFactory;
import java.util.List;

import javax.management.MBeanServer;

import lombok.extern.slf4j.Slf4j;

import net.sf.ehcache.CacheManager;
import net.sf.ehcache.management.ManagementService;

import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class CacheMBeanRegistration implements ApplicationListener<ContextRefreshedEvent>
{
	private boolean doInit = true;

	@Override
	public void onApplicationEvent(ContextRefreshedEvent contextRefreshedEvent)
	{
		if (doInit)
		{
			LOG.info("Start registration of MBeans of the ehcache");
			List<CacheManager> cacheManagers = CacheManager.ALL_CACHE_MANAGERS;
			for (CacheManager cacheManager : cacheManagers)
			{
				try
				{
					MBeanServer mBeanServer = ManagementFactory.getPlatformMBeanServer();
					if (mBeanServer != null)
					{
						ManagementService.registerMBeans(cacheManager, mBeanServer, false, false, false, true);
					}
				}
				catch (Exception e)
				{
					LOG.error(e.getMessage(), e);
				}
			}
			doInit = false;
		}
	}
}
