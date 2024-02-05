
package nl.rivm.screenit.main.web.gebruiker.login;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Application;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.link.BookmarkablePageLink;

public class PasswordMustChangePage extends LoginBasePage
{

	private static final long serialVersionUID = 1L;

	public PasswordMustChangePage(InstellingGebruiker instellingGebruiker)
	{
		setDefaultModel(ModelUtil.sModel(instellingGebruiker));
		add(new PasswordChangePanel("panel", instellingGebruiker.getMedewerker())
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onWachtwoordChanged(AjaxRequestTarget target, Gebruiker gebruiker)
			{
				InstellingGebruiker instellingGebruiker = (InstellingGebruiker) PasswordMustChangePage.this.getDefaultModelObject();
				ScreenitSession session = ScreenitSession.get();
				Component pageForInstellingGebruiker = session.getPageForInstellingGebruiker(instellingGebruiker);
				if (pageForInstellingGebruiker != null)
				{
					setResponsePage((WebPage) pageForInstellingGebruiker);
				}
				else if (session.getFeedbackMessages().isEmpty())
				{
					error(getString("error.nietvoldoende.rechten"));
				}
			}

		});

		add(new BookmarkablePageLink<Void>("naarinlogpagina", Application.get().getHomePage()));
	}
}
