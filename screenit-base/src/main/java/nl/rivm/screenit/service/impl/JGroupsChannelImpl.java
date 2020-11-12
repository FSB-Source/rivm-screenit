
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

import java.net.InetAddress;
import java.util.List;

import nl.rivm.screenit.service.JGroupsChannel;

import org.apache.commons.lang.StringUtils;
import org.jgroups.JChannel;
import org.jgroups.PhysicalAddress;
import org.jgroups.protocols.TCP;
import org.jgroups.protocols.TCPPING;
import org.jgroups.util.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service(value = "jGroupsChannel")
public class JGroupsChannelImpl implements JGroupsChannel
{

	private static final Logger LOG = LoggerFactory.getLogger(JGroupsChannelImpl.class);

	@Autowired
	private String applicatieInstantie;

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
					channel = new JChannel(JGroupsChannelImpl.class.getResource("/tcp.xml"));

					TCP tcpProtocol = (TCP) channel.getProtocolStack().findProtocol(TCP.class);
					tcpProtocol.setBindPort(jgroupsCommonBindPort);
					tcpProtocol.setBindAddress(InetAddress.getByName(jgroupsBindIP));

					TCPPING tcpPingProtocol = (TCPPING) channel.getProtocolStack().findProtocol(TCPPING.class);
					List<PhysicalAddress> initialHosts = Util.parseCommaDelimitedHosts(jgroupsCommonIPPorts, 0);
					tcpProtocol.setPortRange(initialHosts.size());
					tcpPingProtocol.setInitialHosts(initialHosts);

					channel.setName("jgCommon-" + applicatieInstantie.toLowerCase());
					channel.connect("jgCommon-GROUP");
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
