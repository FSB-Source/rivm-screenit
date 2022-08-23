package nl.rivm.screenit.mamma.planning.filter;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.mamma.planning.PlanningRestConstants;
import nl.rivm.screenit.mamma.planning.index.PlanningStatusIndex;
import nl.rivm.screenit.model.mamma.enums.MammaPlanningStatus;
import nl.rivm.screenit.util.EnvironmentUtil;

import org.springframework.http.HttpStatus;

@Slf4j
public class PlanningControllerSynchronizedRequestFilter implements Filter
{

	private static final Integer MAX_WACHTTIJD_LANGE_PLANNING_ACTIES = EnvironmentUtil.getIntegerEnvironmentVariable("MAX_WACHTTIJD_LANGE_PLANNING_ACTIES", 50);

	private final Semaphore semaphore = new Semaphore(1);

	@Override
	public void init(FilterConfig filterConfig)
	{
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException
	{
		LOG.trace("Started");
		String path = ((HttpServletRequest) request).getRequestURI();
		if (path.startsWith("/" + PlanningRestConstants.C_STATUS))
		{
			chain.doFilter(request, response);
		}
		else
		{
			try
			{
				boolean acquired = semaphore.tryAcquire(MAX_WACHTTIJD_LANGE_PLANNING_ACTIES, TimeUnit.SECONDS);
				if (acquired)
				{
					try
					{
						if (PlanningStatusIndex.get() != MammaPlanningStatus.OPERATIONEEL && !path.equals(
							String.format("/%1$s/%2$s", PlanningRestConstants.C_ACTIE, PlanningRestConstants.C_READMODEL)))
						{
							LOG.info("Request naar {} afgebroken omdat de planning niet beschikbaar is", path);
							var httpServletResponse = (HttpServletResponse) response;
							httpServletResponse.sendError(HttpServletResponse.SC_SERVICE_UNAVAILABLE);
						}
						else
						{
							chain.doFilter(request, response);
						}
					}
					finally
					{
						semaphore.release();
					}
				}
				else
				{
					LOG.info("locked");
					HttpServletResponse httpServletResponse = (HttpServletResponse) response;
					httpServletResponse.setStatus(HttpStatus.LOCKED.value());
				}

			}
			catch (InterruptedException e)
			{
				LOG.error("Fout bij wachten op kunnen uitvoeren van een taak", e);
				HttpServletResponse httpServletResponse = (HttpServletResponse) response;
				httpServletResponse.sendError(HttpServletResponse.SC_SERVICE_UNAVAILABLE);
			}
		}
		LOG.trace("Ended");
	}

	@Override
	public void destroy()
	{
	}

}
