package nl.rivm.screenit.wsb.interceptors;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.io.IOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import org.apache.cxf.binding.soap.interceptor.SoapHeaderInterceptor;
import org.apache.cxf.configuration.security.AuthorizationPolicy;
import org.apache.cxf.endpoint.Endpoint;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.Message;
import org.apache.cxf.transport.Conduit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Slf4j
public class BasicAuthAuthorizationInterceptor extends SoapHeaderInterceptor
{
	private String username;
	private String password;

	@Override
	public void handleMessage(Message message) throws Fault
	{
		super.handleMessage(message);

		AuthorizationPolicy policy = message.get(AuthorizationPolicy.class);

		if (policy == null)
		{
			if (LOG.isDebugEnabled())
			{
				LOG.debug("User attempted to log in with no credentials");
			}
			sendErrorResponse(message, HttpURLConnection.HTTP_UNAUTHORIZED);
			return;
		}

		if (!username.equals(policy.getUserName()) || !password.equals(policy.getPassword()))
		{
			sendErrorResponse(message, HttpURLConnection.HTTP_FORBIDDEN);
		}
	}

	private void sendErrorResponse(Message message, int responseCode)
	{
		Message outMessage = getOutMessage(message);
		outMessage.put(Message.RESPONSE_CODE, responseCode);

		Map<String, List<String>> responseHeaders = (Map<String, List<String>>) message.get(Message.PROTOCOL_HEADERS);
		if (responseHeaders != null)
		{
			responseHeaders.put("WWW-Authenticate", Arrays.asList(new String[] { "Basic realm=realm" }));
			responseHeaders.put("Content-Length", Arrays.asList(new String[] { "0" }));
		}
		message.getInterceptorChain().abort();
		try
		{
			getConduit(message).prepare(outMessage);
			close(outMessage);
		}
		catch (IOException e)
		{
			LOG.warn(e.getMessage(), e);
		}
	}

	private Message getOutMessage(Message inMessage)
	{
		Exchange exchange = inMessage.getExchange();
		Message outMessage = exchange.getOutMessage();
		if (outMessage == null)
		{
			Endpoint endpoint = exchange.get(Endpoint.class);
			outMessage = endpoint.getBinding().createMessage();
			exchange.setOutMessage(outMessage);
		}
		outMessage.putAll(inMessage);
		return outMessage;
	}

	private Conduit getConduit(Message inMessage) throws IOException
	{
		Exchange exchange = inMessage.getExchange();
		Conduit conduit = exchange.getDestination().getBackChannel(inMessage);
		exchange.setConduit(conduit);
		return conduit;
	}

	private void close(Message outMessage) throws IOException
	{
		OutputStream os = outMessage.getContent(OutputStream.class);
		os.flush();
		os.close();
	}

	public void setUsername(String username)
	{
		this.username = username;
	}

	public void setPassword(String password)
	{
		this.password = password;
	}
}
