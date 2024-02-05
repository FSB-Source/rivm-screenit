package nl.rivm.screenit.batch.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

import com.jcraft.jsch.ChannelDirectTCPIP;
import com.jcraft.jsch.Proxy;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SocketFactory;

public class SSHProxy implements Proxy
{

	public SSHProxy(Session gateway)
	{
		this.gateway = gateway;
	}

	private final Session gateway;

	private ChannelDirectTCPIP channel;

	private InputStream iStream;

	private OutputStream oStream;

	@Override
	public void close()
	{
		channel.disconnect();
	}

	@Override
	public void connect(SocketFactory ignore, String host, int port, int timeout) throws Exception
	{
		channel = (ChannelDirectTCPIP) gateway.openChannel("direct-tcpip");
		channel.setHost(host);
		channel.setPort(port);

		iStream = channel.getInputStream();
		oStream = channel.getOutputStream();
		channel.connect();
	}

	@Override
	public InputStream getInputStream()
	{
		return iStream;
	}

	@Override
	public OutputStream getOutputStream()
	{
		return oStream;
	}

	@Override
	public Socket getSocket()
	{

		return null;
	}

}
