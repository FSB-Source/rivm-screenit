package nl.rivm.screenit.main.web.security;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.base.ScreenitContext;

import org.apache.shiro.SecurityUtils;
import org.apache.shiro.subject.Subject;
import org.apache.wicket.Application;
import org.apache.wicket.Component;
import org.apache.wicket.Page;
import org.apache.wicket.RestartResponseAtInterceptPageException;
import org.apache.wicket.RestartResponseException;
import org.apache.wicket.authorization.IUnauthorizedComponentInstantiationListener;

public class ScreenitShiroUnauthorizedComponentListener implements IUnauthorizedComponentInstantiationListener
{
	private final Class<? extends Page> loginPage;

	private final Class<? extends Page> unauthorizedPage;

	private ScreenitAnnotationsShiroAuthorizationStrategy annotationStrategy = null;

	public ScreenitShiroUnauthorizedComponentListener(final Class<? extends Page> loginPage, final Class<? extends Page> unauthorizedPage,
		final ScreenitAnnotationsShiroAuthorizationStrategy s)
	{
		this.loginPage = loginPage;
		this.unauthorizedPage = unauthorizedPage;
		annotationStrategy = s;
	}

	public ScreenitAnnotationsShiroAuthorizationStrategy getAnnotationStrategy()
	{
		return annotationStrategy;
	}

	protected String getMessage(final String key, final SecurityConstraint anno, final Component comp)
	{
		return key;
	}

	@Override
	public void onUnauthorizedInstantiation(final Component component)
	{
		final Subject subject = SecurityUtils.getSubject();
		final boolean notLoggedIn = subject.getPrincipal() == null;
		final Class<? extends Page> page;
		if (notLoggedIn)
		{
			if (component.getPage() instanceof BasePage)
			{
				BasePage basePage = (BasePage) component.getPage();
				if (basePage.getContext() == ScreenitContext.GEBRUIKER)
				{
					page = Application.get().getHomePage();
				}
				else
				{
					throw new IllegalStateException("No context found in basepage");
				}
			}
			else
			{
				page = loginPage;
			}
		}
		else
		{

			page = unauthorizedPage;
		}

		if (notLoggedIn)
		{

			throw new RestartResponseAtInterceptPageException(page);
		}

		throw new RestartResponseException(page);
	}

	public void setAnnotationStrategy(final ScreenitAnnotationsShiroAuthorizationStrategy annotationStrategy)
	{
		this.annotationStrategy = annotationStrategy;
	}

}
