package nl.rivm.screenit.cache;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Properties;

import javax.naming.InitialContext;
import javax.naming.NamingException;

import lombok.extern.slf4j.Slf4j;

import net.sf.ehcache.CacheManager;
import net.sf.ehcache.distribution.CacheManagerPeerProvider;
import net.sf.ehcache.distribution.CacheManagerPeerProviderFactory;
import net.sf.ehcache.distribution.jgroups.JGroupsCacheManagerPeerProvider;

import org.apache.commons.lang3.StringUtils;
import org.jgroups.Global;

@Slf4j
public class JNDIJGroupsCacheManagerPeerProviderFactory extends CacheManagerPeerProviderFactory
{
	public static String initialHosts;

	public static String bindIp;

	public static Integer bindPort;

	public static String applicationInstance;

	@Override
	public CacheManagerPeerProvider createCachePeerProvider(CacheManager cacheManager, Properties properties)
	{
		try
		{
			InitialContext context = new InitialContext();
			initialHosts = (String) context.lookup("jgroupsEhcacheIPPorts");
			bindIp = (String) context.lookup("jgroupsBindIP");
			bindPort = Integer.valueOf((String) context.lookup("jgroupsEhcacheBindPort"));
			applicationInstance = (String) context.lookup("applicationInstance");
		}
		catch (NamingException e)
		{
			LOG.warn(e.getMessage());
		}

		if (StringUtils.isBlank(initialHosts))
		{
			initialHosts = "127.0.0.1[12001]";
		}
		if (StringUtils.isBlank(bindIp))
		{
			bindIp = "127.0.0.1"; 
		}
		if (bindPort == null)
		{
			bindPort = 7800;
		}
		LOG.info("External bind IP: " + bindIp + "; bind port: " + bindPort + "; initialHosts: " + initialHosts);

		if (System.getProperty(Global.BIND_PORT) == null)
		{
			System.getProperties().put(Global.BIND_PORT, bindPort.toString());
		}
		if (System.getProperty(Global.EXTERNAL_ADDR) == null && !bindIp.equals("127.0.0.1") && !bindIp.equals("localhost"))
		{
			System.getProperties().put(Global.EXTERNAL_ADDR, bindIp);
		}
		if (System.getProperty(Global.GOSSIP_ROUTER) == null)
		{
			System.getProperties().put(Global.GOSSIP_ROUTER, initialHosts);
		}
		return new JGroupsCacheManagerPeerProvider(applicationInstance.toLowerCase(), cacheManager,
			JNDIJGroupsCacheManagerPeerProviderFactory.class.getResource("/jg-sit-tcp.xml"));
	}
}
