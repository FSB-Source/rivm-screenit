
package nl.rivm.screenit.batch;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;
import net.sf.ehcache.config.Configuration;
import net.sf.ehcache.config.ConfigurationFactory;

import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.Lifecycle;
import org.springframework.jmx.export.annotation.ManagedAttribute;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedResource;

@ManagedResource
public class ImprovedSimpleEhCacheInterceptor implements MethodInterceptor, InitializingBean, DisposableBean, Lifecycle
{

	private Cache cache;

	private CacheManager manager;

	private volatile boolean caching = true;

	private long timeout = 60;

	private String name = "simple";

	public void setTimeout(int timeout)
	{
		this.timeout = timeout;
	}

	public void setCacheName(String name)
	{
		this.name = name;
	}

	@Override
	public void afterPropertiesSet() throws Exception
	{
		Configuration config = ConfigurationFactory.parseConfiguration();
		config.setUpdateCheck(false);
		manager = CacheManager.create(config);
		cache = new Cache(name, 10000, true, false, timeout, 0);
		manager.addCache(cache);
	}

	@Override
	public void destroy() throws Exception
	{
		if (manager != null)
		{
			manager.removalAll();
			manager.shutdown();
		}
	}

	@Override
	public Object invoke(MethodInvocation invocation) throws Throwable
	{
		Serializable key = getKey(invocation);
		Element element = cache.get(key);
		Object value = null;
		if (caching && (element == null || element.isExpired()))
		{
			cache.remove(key);
			Object old = element == null ? null : element.getValue();
			value = invocation.proceed();
			if (cacheable(value, old))
			{
				cache.putIfAbsent(new Element(key, value));
			}
		}
		else
		{
			value = element.getObjectValue();
		}
		return value;
	}

	@SuppressWarnings("rawtypes")
	private boolean cacheable(Object value, Object old)
	{
		if (value == null)
		{
			return false;
		}
		if (old != null)
		{
			return true;
		}
		if (value instanceof Collection)
		{
			if (((Collection) value).isEmpty())
			{
				return false;
			}

		}
		if (value instanceof Map)
		{
			if (((Map) value).isEmpty())
			{
				return false;
			}
		}
		if (value.getClass().isArray())
		{
			if (((Object[]) value).length == 0)
			{
				return false;
			}
		}
		return true;
	}

	private Serializable getKey(MethodInvocation invocation)
	{
		return invocation.getMethod().getName() + Arrays.asList(invocation.getArguments());
	}

	@Override
	@ManagedOperation
	public void start()
	{
		caching = true;
	}

	@Override
	@ManagedOperation
	public void stop()
	{
		caching = false;
	}

	@Override
	@ManagedAttribute
	public boolean isRunning()
	{
		return caching;
	}

}
