package nl.rivm.screenit.main.web.client.base;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.digid.ClientportaalDigidLoginPage;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;

public class ClientLoginFoutPage extends ClientBasePage
{

	private static final long serialVersionUID = 1L;

	public ClientLoginFoutPage(String error)
	{
		add(new Label("onbekendeDashboardMelding", error).setEscapeModelStrings(false));
		add(new Link<Void>("terug")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				String url = ScreenitSession.get().getAfkomstigURLRegioCode();
				String regioCode = ScreenitSession.get().getRegioCode();
				ScreenitSession.get().logout();
				PageParameters pageParameters = new PageParameters();
				if (StringUtils.isNotBlank(url))
				{
					pageParameters.add("url", url);
				}
				if (StringUtils.isNotBlank(regioCode))
				{
					pageParameters.add("regiocode", regioCode);
				}
				setResponsePage(ClientportaalDigidLoginPage.class, pageParameters);
			}

		});
	}

	@Override
	public IModel<String> getPageName()
	{
		return new Model<>("");
	}

	@Override
	public ClientHoofdMenuitem getHoofdMenuitem()
	{
		return ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK;
	}

}
