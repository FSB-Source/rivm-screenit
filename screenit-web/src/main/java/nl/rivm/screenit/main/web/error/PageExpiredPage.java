
package nl.rivm.screenit.main.web.error;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.main.web.ScreenitApplication;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.base.ScreenitContext;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class PageExpiredPage extends BasePage
{
	@SpringBean(name = "applicationUrl")
	private String applicationUrl;

	@SpringBean(name = "portaalUrl")
	private String portaalUrl;

	public PageExpiredPage()
	{
		add(new WebMarkupContainer("opnieuwInloggenAlsClient").add(new AttributeAppender("href", portaalUrl + "/" + ScreenitApplication.CLIENTPORTAAL_MOUNT)));

		add(new WebMarkupContainer("opnieuwInloggen").add(new AttributeAppender("href", applicationUrl)));

		String uitwisselportaalUrl = applicationUrl;
		if (!uitwisselportaalUrl.endsWith("/"))
		{
			uitwisselportaalUrl += "/";
		}
		uitwisselportaalUrl += ScreenitApplication.UITWISSELPORTAAL_MOUNT;
		uitwisselportaalUrl = uitwisselportaalUrl.replace(Constants.BASE_SUBDOMEIN_MEDEWERKERPORTAAL, ScreenitApplication.UITWISSELPORTAAL_MOUNT);
		add(new WebMarkupContainer("opnieuwInloggenUitwisselportaal").add(new AttributeAppender("href", uitwisselportaalUrl)).setVisible(false));
	}

	@Override
	public ScreenitContext getContext()
	{
		return null;
	}

	@Override
	public void refreshFeedback(AjaxRequestTarget target)
	{

	}

}
