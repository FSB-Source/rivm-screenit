package nl.rivm.screenit.main.web.filter;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.IOException;
import java.net.HttpCookie;
import java.util.Collection;
import java.util.List;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.SessionCookieConfig;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import nl.rivm.screenit.main.service.UzipasLoginService;
import nl.rivm.screenit.main.web.ScreenitApplication;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CookieFilter implements Filter
{
	private static final Logger LOG = LoggerFactory.getLogger(CookieFilter.class);

	private static String SET_COOKIE_HEADER = "Set-Cookie";

	@Override
	public void init(FilterConfig filterConfig) throws ServletException
	{
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException
	{
		if (response instanceof HttpServletResponse)
		{
			HttpServletResponse httpServletResponse = (HttpServletResponse) response;
			SessionCookieConfig sessionCookieConfig = request.getServletContext().getSessionCookieConfig();

			if (httpServletResponse.containsHeader(SET_COOKIE_HEADER))
			{
				Collection<String> headers = httpServletResponse.getHeaders(SET_COOKIE_HEADER);
				String jsessionCookieString = headers.stream().filter(h -> h.startsWith(sessionCookieConfig.getName())).findFirst().orElse(null);
				if (StringUtils.isNotBlank(jsessionCookieString))
				{
					List<HttpCookie> cookies = HttpCookie.parse(jsessionCookieString);
					if (cookies.size() == 1)
					{
						HttpCookie cookie = cookies.get(0);
						Cookie newCookie = new Cookie(cookie.getName(), cookie.getValue());
						newCookie.setComment(cookie.getComment());
						newCookie.setHttpOnly(cookie.isHttpOnly());
						newCookie.setMaxAge((int) cookie.getMaxAge());
						newCookie.setPath(cookie.getPath());
						newCookie.setSecure(cookie.getSecure());
						newCookie.setVersion(cookie.getVersion());
						boolean fromUitwisselportaal = false;
						String path = ((HttpServletRequest) request).getPathInfo();
						if (StringUtils.isNotBlank(path))
						{
							fromUitwisselportaal = path.contains(ScreenitApplication.UITWISSELPORTAAL_MOUNT);
						}
						UzipasLoginService uzipasLoginService = SpringBeanProvider.getInstance().getBean(UzipasLoginService.class);
						newCookie.setDomain(uzipasLoginService.currentApplicationDomain(fromUitwisselportaal));
						httpServletResponse.addCookie(newCookie);
					}
				}
			}
		}
		chain.doFilter(request, response);
	}

	@Override
	public void destroy()
	{

	}
}
