package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.net.InetSocketAddress;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.JGroupsChannel;

import org.apache.commons.lang.StringUtils;
import org.jgroups.Global;
import org.jgroups.JChannel;
import org.jgroups.protocols.TCP;
import org.jgroups.protocols.TCPGOSSIP;
import org.jgroups.util.Util;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service(value = "jGroupsChannel")
@Profile("!test")
@Slf4j
public class JGroupsChannelImpl implements JGroupsChannel
{

	@Autowired
	private String applicationInstance;

	@Autowired
	private Integer jgroupsCommonBindPort;

	@Autowired
	private String jgroupsCommonIPPorts;

	@Autowired
	private String jgroupsBindIP;

	private JChannel channel = null;

	@Override
	public JChannel getChannel()
	{
		if (channel == null)
		{
			if (StringUtils.isNotBlank(jgroupsCommonIPPorts) && jgroupsCommonBindPort > 0)
			{
				try
				{
					LOG.info("External bind IP: " + jgroupsBindIP + "; bind port: " + jgroupsCommonBindPort + "; initialHosts: " + jgroupsCommonIPPorts);
					if (System.getProperty(Global.EXTERNAL_ADDR) == null && !jgroupsBindIP.equals("127.0.0.1") && !jgroupsBindIP.equals("localhost"))
					{
						System.getProperties().put(Global.EXTERNAL_ADDR, jgroupsBindIP);
					}

					channel = new JChannel(JGroupsChannelImpl.class.getResource("/jg-sit-tcp.xml"));

					TCP tcpProtocol = channel.getProtocolStack().findProtocol(TCP.class);
					tcpProtocol.setBindPort(jgroupsCommonBindPort);

					List<InetSocketAddress> initialHosts = Util.parseCommaDelimitedHosts2(jgroupsCommonIPPorts, 0);
					TCPGOSSIP tcpGossipProtocol = channel.getProtocolStack().findProtocol(TCPGOSSIP.class);
					tcpGossipProtocol.setInitialHosts(initialHosts);

					channel.setName("jgCommon-" + applicationInstance.toLowerCase());
					channel.connect("jgCommon-GROUP");
					LOG.info(channel.toString(true));
				}
				catch (Exception e)
				{
					LOG.error("Error tijdens init van jqroups chanel", e);
				}
			}
		}
		return channel;
	}
}
