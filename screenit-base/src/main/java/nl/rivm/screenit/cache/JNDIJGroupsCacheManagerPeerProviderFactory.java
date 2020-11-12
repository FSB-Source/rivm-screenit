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
import net.sf.ehcache.distribution.jgroups.JGroupsCacheManagerPeerProvider;
import net.sf.ehcache.distribution.jgroups.JGroupsCacheManagerPeerProviderFactory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JNDIJGroupsCacheManagerPeerProviderFactory extends JGroupsCacheManagerPeerProviderFactory
{
	
	private static final Logger LOG = LoggerFactory.getLogger(JNDIJGroupsCacheManagerPeerProviderFactory.class);

	@Override
	public CacheManagerPeerProvider createCachePeerProvider(CacheManager cacheManager, Properties properties)
	{
		String initialHosts = "";
		String bindIp = "";
		String bindPort = "";
		try
		{
			InitialContext initialContext = new InitialContext();
			initialHosts = (String) initialContext.lookup("jgroupsEhcacheIPPorts");
			bindIp = (String) initialContext.lookup("jgroupsBindIP");
			bindPort = (String) initialContext.lookup("jgroupsEhcacheBindPort");

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

		String connect = "TCP(bind_addr=" + bindIp + ";bind_port=" + bindPort + ";recv_buf_size=5M;send_buf_size=5M;"
			+ "max_bundle_size=64K;max_bundle_timeout=30;sock_conn_timeout=300;timer_type=new3;"
			+ "timer.min_threads=4;timer.max_threads=10;timer.keep_alive_time=3000;timer.queue_max_size=500;"
			+ "thread_pool.enabled=true;thread_pool.min_threads=2;thread_pool.max_threads=8;thread_pool.keep_alive_time=5000;"
			+ "thread_pool.queue_enabled=true;thread_pool.queue_max_size=10000;thread_pool.rejection_policy=discard;"
			+ "oob_thread_pool.enabled=true;oob_thread_pool.min_threads=1;oob_thread_pool.max_threads=8;oob_thread_pool.keep_alive_time=5000;"
			+ "oob_thread_pool.queue_enabled=false;oob_thread_pool.queue_max_size=100;oob_thread_pool.rejection_policy=discard):\n";

		connect += "TCPPING(initial_hosts=" + initialHosts + ";port_range=0):\n";

		connect += "MERGE3(max_interval=100000;min_interval=20000):\n" + "FD_SOCK():\n" + "FD(timeout=10000;max_tries=5):\n" + "VERIFY_SUSPECT(timeout=1500):\n"
			+ "pbcast.NAKACK(use_mcast_xmit=false;retransmit_timeout=300,600,1200,2400,4800;" + "discard_delivered_msgs=true):\n" 
			+ "UNICAST3():\n" 
			+ "pbcast.STABLE(stability_delay=1000;desired_avg_gossip=50000;max_bytes=400000):\n" + "pbcast.GMS(print_local_addr=true;join_timeout=3000;view_bundling=true):\n"
			+ "FC(max_credits=2000000;min_threshold=0.10):\n" 
			+ "FRAG2(frag_size=60000)";

		return new JGroupsCacheManagerPeerProvider(cacheManager, connect);
	}
}
