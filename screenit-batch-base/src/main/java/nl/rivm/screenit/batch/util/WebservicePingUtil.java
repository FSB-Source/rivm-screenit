
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

import java.io.IOException;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import org.apache.commons.collections.CollectionUtils;
import org.apache.http.HttpEntity;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.ResponseHandler;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.config.RequestConfig.Builder;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class WebservicePingUtil
{
	private static final Logger LOG = LoggerFactory.getLogger(WebservicePingUtil.class);

	private WebservicePingUtil()
	{

	}

	public static boolean ping(String url, List<String> markers, StringBuilder message)
	{
		CloseableHttpClient httpclient = HttpClients.createDefault();
		boolean hasWsdlVerified = false;
		if (httpclient != null)
		{
			try
			{
				HttpGet httpget = new HttpGet(url);
				int timeout = 10000; 
				Builder customRequestConfigBuilder = RequestConfig.custom().setConnectionRequestTimeout(timeout).setSocketTimeout(timeout)
					.setConnectTimeout(timeout);
				httpget.setConfig(customRequestConfigBuilder.build());

				ResponseHandler<String> responseHandler = response ->
				{
					int status = response.getStatusLine().getStatusCode();
					if (status >= 200 && status < 300)
					{
						HttpEntity entity = response.getEntity();
						return entity != null ? EntityUtils.toString(entity) : null;
					}
					else
					{
						throw new ClientProtocolException("Unexpected response status: " + status);
					}
				};

				TimerTask task = new TimerTask()
				{
					@Override
					public void run()
					{
						if (httpget != null)
						{
							httpget.abort();
						}
					}
				};
				new Timer(true).schedule(task, 6 * timeout);

				String responseBody = httpclient.execute(httpget, responseHandler);
				hasWsdlVerified = true;
				if (CollectionUtils.isNotEmpty(markers))
				{
					for (String marker : markers)
					{
						hasWsdlVerified &= responseBody.contains(marker);
					}
				}
				if (!hasWsdlVerified)
				{
					message.append(responseBody);
				}
			}
			catch (IOException e)
			{
				LOG.error("Fout bij pingen op webservice " + url, e);
				message.append("Fout: " + e.getMessage());
			}
			finally
			{
				try
				{
					httpclient.close();
				}
				catch (IOException e)
				{
					LOG.error("Fout bij sluiten httpclient ", e);
				}
			}
		}
		return hasWsdlVerified;

	}
}
