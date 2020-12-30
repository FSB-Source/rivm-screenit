
package nl.rivm.screenit.main.web.gebruiker.login;

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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.login.uzipas.UzipasLoginMethodPage;
import nl.rivm.screenit.model.enums.InlogMethode;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.util.cookies.CookieUtils;

public class MedewerkerLoginMethodPage extends LoginBasePage
{
	public static final String FEEDBACK = "feedback";

	private static final long serialVersionUID = 1L;

	public MedewerkerLoginMethodPage()
	{
		this(true);
	}

	public MedewerkerLoginMethodPage(PageParameters pageParameters)
	{
		this(false);
		ScreenitSession.get().info(pageParameters.get(FEEDBACK).toString());
	}

	public MedewerkerLoginMethodPage(boolean redirect)
	{
		add(new BookmarkablePageLink<>("yubikeyLogin", YubipasLoginPage.class));
		add(new BookmarkablePageLink<>("usernamePassword", MedewerkerLoginPage.class));
		add(new BookmarkablePageLink<>("uzipasLogin", UzipasLoginMethodPage.class, new PageParameters().add(PAGE_PARAMETER_UITWISSELPORTAAL, Boolean.FALSE)));

		String loginMethode = new CookieUtils().load(Constants.COOKIE_KEY_LOGIN_METHOD);
		if (StringUtils.isNotBlank(loginMethode) && redirect)
		{
			switch (InlogMethode.valueOf(loginMethode))
			{
			case GEBRUIKERSNAAM_WACHTWOORD:
				setResponsePage(MedewerkerLoginPage.class);
				break;
			case YUBIKEY:
				setResponsePage(YubipasLoginPage.class);
				break;
			case UZIPAS:
				setResponsePage(UzipasLoginMethodPage.class, new PageParameters().add(PAGE_PARAMETER_UITWISSELPORTAAL, Boolean.FALSE));
				break;
			default:
				break;
			}
		}
	}
}
