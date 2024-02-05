package nl.rivm.screenit.main.web.filter;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import lombok.extern.slf4j.Slf4j;

import org.apache.wicket.csp.CSPHeaderConfiguration;
import org.apache.wicket.csp.CSPHeaderMode;
import org.apache.wicket.protocol.http.WebApplication;
import org.apache.wicket.request.http.WebResponse;

import static org.apache.wicket.ThreadContext.getRequestCycle;

@Slf4j
public class SecurityHeadersFilter implements Filter
{
	private static final String DEFAULT_CONTENT_SECURITY_POLICY_WITH_UNSAFE_SCRIPT_INLINE = "form-action 'self'; frame-ancestors 'self'; default-src 'self'; " +
		"script-src 'self' 'unsafe-inline' 'unsafe-eval'; style-src 'self' 'unsafe-inline'; object-src 'self'";

	public static void allowUnsafeInlineSecurityPolicy(WebResponse response)
	{
		response.setHeader("Content-Security-Policy", SecurityHeadersFilter.DEFAULT_CONTENT_SECURITY_POLICY_WITH_UNSAFE_SCRIPT_INLINE);
	}

	@Override
	public void init(FilterConfig filterConfig) throws ServletException
	{
	}

	@Override
	public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws IOException, ServletException
	{
		HttpServletRequest request = (HttpServletRequest) servletRequest;
		HttpServletResponse response = (HttpServletResponse) servletResponse;

		LOG.debug("Invoking SecurityHeadersFilter for " + request.getRequestURI());

		response.setHeader("X-Frame-Options", "sameorigin");
		response.setHeader("X-XSS-Protection", "1; mode=block");
		response.setHeader("X-Content-Type-Options", "nosniff");
		response.setHeader("Referrer-Policy", "same-origin");

		filterChain.doFilter(servletRequest, servletResponse);
	}

	public static void allowExtraConnectSrcInContentSecurityPolicy(WebResponse response, String extraConnectSrc)
	{
			var cspSettings = WebApplication.get().getCspSettings();
			CSPHeaderConfiguration cspHeaderConfiguration = cspSettings.getConfiguration().get(CSPHeaderMode.BLOCKING);
			String headerValue = cspHeaderConfiguration.renderHeaderValue(cspSettings, getRequestCycle());

			headerValue = headerValue.replace("connect-src 'self'", "");
			String connectieString = "connect-src 'self' " + extraConnectSrc;

			String cspString = String.join(";", headerValue, connectieString);

			response.setHeader("Content-Security-Policy", cspString);
	}

	@Override
	public void destroy()
	{
	}
}
