package nl.rivm.screenit.cache;

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

import java.util.Properties;

import javax.naming.InitialContext;
import javax.naming.NamingException;

import net.sf.ehcache.CacheManager;
import net.sf.ehcache.distribution.CacheManagerPeerProvider;
import net.sf.ehcache.distribution.CacheManagerPeerProviderFactory;
import net.sf.ehcache.distribution.jgroups.JGroupsCacheManagerPeerProvider;

import org.jgroups.Global;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JNDIJGroupsCacheManagerPeerProviderFactory extends CacheManagerPeerProviderFactory
{
	private static final Logger LOG = LoggerFactory.getLogger(JNDIJGroupsCacheManagerPeerProviderFactory.class);

	@Override
	public CacheManagerPeerProvider createCachePeerProvider(CacheManager cacheManager, Properties properties)
	{
		String initialHosts = "";
		String bindIp = "";
		String bindPort = "";
		String applicationInstance = "";
		try
		{
			InitialContext initialContext = new InitialContext();
			initialHosts = (String) initialContext.lookup("jgroupsEhcacheIPPorts");
			bindIp = (String) initialContext.lookup("jgroupsBindIP");
			bindPort = (String) initialContext.lookup("jgroupsEhcacheBindPort");
			applicationInstance = (String) initialContext.lookup("applicationInstance");

			if (initialHosts == null)
			{
				initialHosts = "127.0.0.1[7800]";
			}
			if (bindIp == null)
			{
				bindIp = "127.0.0.1"; 
			}
			if (bindPort == null)
			{
				bindPort = "7800";
			}
		}
		catch (NamingException e)
		{
			LOG.error(e.getMessage(), e);
			throw new IllegalStateException(e.getMessage(), e);
		}
		LOG.info("External bind IP: " + bindIp + "; bind port: " + bindPort + "; initialHosts: " + initialHosts);

		if (System.getProperty(Global.BIND_PORT) == null)
		{
			System.getProperties().put(Global.BIND_PORT, bindPort);
		}
		if (System.getProperty(Global.EXTERNAL_ADDR) == null && !bindIp.equals("127.0.0.1") && !bindIp.equals("localhost"))
		{
			System.getProperties().put(Global.EXTERNAL_ADDR, bindIp);
		}
		if (System.getProperty(Global.TCPPING_INITIAL_HOSTS) == null)
		{
			System.getProperties().put(Global.TCPPING_INITIAL_HOSTS, initialHosts);
		}
		return new JGroupsCacheManagerPeerProvider(applicationInstance.toLowerCase(), cacheManager,
			JNDIJGroupsCacheManagerPeerProviderFactory.class.getResource("/jg-sit-tcp.xml"));
	}
}
