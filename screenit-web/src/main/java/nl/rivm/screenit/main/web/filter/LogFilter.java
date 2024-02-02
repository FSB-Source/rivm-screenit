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
import java.util.Locale;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.LocaleResolver;
import nl.rivm.screenit.main.service.LocaleResolverService;
import nl.rivm.screenit.main.web.ScreenitApplication;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;

import org.apache.wicket.Session;
import org.hibernate.SessionFactory;
import org.slf4j.MDC;
import org.springframework.orm.hibernate5.SessionHolder;
import org.springframework.orm.hibernate5.support.OpenSessionInViewFilter;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.WebApplicationContextUtils;

@Slf4j
public class LogFilter implements Filter
{

	private String sessionFactoryBeanName = OpenSessionInViewFilter.DEFAULT_SESSION_FACTORY_BEAN_NAME;

	private WebApplicationContext webApplicationContext;

	private LocaleResolverService localeResolverService;

	private static final LocaleResolver DEFAULT_LOCALE_RESOLVER = () -> Locale.ENGLISH;

	@Override
	public void init(FilterConfig filterConfig) throws ServletException
	{
		String sessionFactoryBeanName = filterConfig.getInitParameter("sessionFactoryBeanName");

		if (sessionFactoryBeanName != null)
		{
			this.sessionFactoryBeanName = sessionFactoryBeanName;
		}

		webApplicationContext = WebApplicationContextUtils.getRequiredWebApplicationContext(filterConfig.getServletContext());

		localeResolverService = webApplicationContext.getBean(LocaleResolverService.class);
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException
	{

		final boolean hasTransactionalSession = hasTransactionalSession();

		LocaleResolver localeResolver = DEFAULT_LOCALE_RESOLVER;

		if (hasTransactionalSession && request instanceof HttpServletRequest)
		{

			HttpServletRequest httpRequest = (HttpServletRequest) request;
			HttpSession httpSession = httpRequest.getSession(true);

			String attributeName = ScreenitApplication.getSessionAttributePrefix() + Session.SESSION_ATTRIBUTE_NAME;
			final ScreenitSession session = (ScreenitSession) httpSession.getAttribute(attributeName);

			if (session != null)
			{
				Class<?> loggedInAccountClass = session.getLoggedInAccountClass();
				if (loggedInAccountClass != null)
				{
					String prefix = "";
					if (Gebruiker.class.isAssignableFrom(loggedInAccountClass))
					{
						prefix = "G";
					}
					if (InstellingGebruiker.class.isAssignableFrom(loggedInAccountClass))
					{
						prefix = "M";
					}
					else if (Client.class.isAssignableFrom(loggedInAccountClass))
					{
						prefix = "C";
					}
					MDC.put("A", prefix + session.getLoggedInAccountId());
				}

				localeResolver = session::getLocale;
			}
		}

		try
		{
			localeResolverService.setLocaleResolver(localeResolver);
			chain.doFilter(request, response);
		}
		finally
		{
			localeResolverService.setLocaleResolver(null);
			if (hasTransactionalSession)
			{
				MDC.remove("A");
			}
		}
	}

	private boolean hasTransactionalSession()
	{
		SessionFactory sessionFactory = webApplicationContext.getBean(sessionFactoryBeanName, SessionFactory.class);

		SessionHolder sessionHolder = (SessionHolder) TransactionSynchronizationManager.getResource(sessionFactory);
		return sessionHolder != null && sessionHolder.getSession().isConnected();
	}

	@Override
	public void destroy()
	{

	}
}
