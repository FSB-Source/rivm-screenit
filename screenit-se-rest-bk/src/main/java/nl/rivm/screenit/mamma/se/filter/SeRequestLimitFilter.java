package nl.rivm.screenit.mamma.se.filter;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.io.IOException;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import nl.rivm.screenit.util.EnvironmentUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Stopwatch;

public class SeRequestLimitFilter implements Filter
{
	private static final Logger LOG = LoggerFactory.getLogger(SeRequestLimitFilter.class);

	private static final int DEFAULT_MAX_REQUESTS = 60; 

	private static final int DEFAULT_ACQUIRE_TIMEOUT_SECONDS = 10;

	private final Semaphore semaphore;

	private final int acquireTimeoutSeconds;

	public SeRequestLimitFilter()
	{
		int maxRequests = EnvironmentUtil.getIntegerEnvironmentVariable("SE_REQUEST_LIMIT", DEFAULT_MAX_REQUESTS);
		semaphore = new Semaphore(maxRequests);
		acquireTimeoutSeconds = EnvironmentUtil.getIntegerEnvironmentVariable("SE_REQUEST_ACQUIRE_TIMEOUT", DEFAULT_ACQUIRE_TIMEOUT_SECONDS);
		LOG.info("SeRequestLimitFilter initialized: maxRequestLimit: {}, acquireTimeout: {} seconds", maxRequests, acquireTimeoutSeconds);
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException
	{
		HttpServletRequest httpRequest = (HttpServletRequest) request;
		HttpServletResponse httpResponse = (HttpServletResponse) response;
		LOG.debug("Request started: {} (available:{}, queued:{})", httpRequest.getRequestURI(), semaphore.availablePermits(), semaphore.getQueueLength());
		Stopwatch stopWatch = Stopwatch.createStarted();
		try
		{
			boolean acquired = semaphore.tryAcquire(acquireTimeoutSeconds, TimeUnit.SECONDS);
			if (acquired)
			{
				long wachtTijd = stopWatch.elapsed(TimeUnit.MILLISECONDS);
				try
				{
					chain.doFilter(request, response);
					LOG.info("Response({}) {} (wait:{}, total:{} ms) (available:{}, queued:{})", httpResponse.getStatus(), httpRequest.getRequestURI(), wachtTijd,
						stopWatch.elapsed(TimeUnit.MILLISECONDS), semaphore.availablePermits(), semaphore.getQueueLength());
				}
				finally
				{
					semaphore.release();
				}
			}
			else
			{
				LOG.warn("Te druk voor request: {} (wait:{})", ((HttpServletRequest) request).getRequestURI(), stopWatch.elapsed(TimeUnit.MILLISECONDS));
				httpResponse.sendError(HttpServletResponse.SC_SERVICE_UNAVAILABLE); 
			}
		}
		catch (InterruptedException e)
		{
			LOG.warn("Thread interrupted tijdens wachten beschikbaar slot voor: {}, exceptie: {}", ((HttpServletRequest) request).getRequestURI(), e);
			httpResponse.sendError(HttpServletResponse.SC_SERVICE_UNAVAILABLE);
		}
	}

	@Override
	public void init(FilterConfig filterConfig)
	{
	}

	@Override
	public void destroy()
	{
	}
}
