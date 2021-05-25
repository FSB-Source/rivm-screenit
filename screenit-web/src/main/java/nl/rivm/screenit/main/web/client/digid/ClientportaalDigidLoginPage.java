package nl.rivm.screenit.main.web.client.digid;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.base.ClientBasePage;
import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.service.InstellingService;

import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.google.common.base.Strings;

public class ClientportaalDigidLoginPage extends ClientBasePage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private InstellingService instellingService;

	public ClientportaalDigidLoginPage(final PageParameters parameters)
	{
		String parameterRegioCode = parameters.get("regiocode").toString();
		String parameterUrl = parameters.get("url").toString();
		String url = null;

		if (!Strings.isNullOrEmpty(parameterUrl))
		{
			if (parameterUrl.startsWith("http"))
			{
				url = parameterUrl;
			}
			else
			{
				url = "http://" + parameterUrl;
			}
		}
		ScreenitSession.get().setAfkomstigURLRegioCode(url);
		ScreenitSession.get().setRegioCode(parameterRegioCode);
		setStatelessHint(true);
		add(new Link<Void>("login")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(ScreenitDigidLoginPage.class);
			}
		});
	}

	@Override
	public ClientHoofdMenuitem getHoofdMenuitem()
	{
		return ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK;
	}

	@Override
	protected List<ClientHoofdMenuitem> getMenuItems()
	{
		return Arrays.asList(ClientHoofdMenuitem.MIJNBEVOLKINGSONDERZOEK);
	}

	@Override
	public IModel<String> getPageName()
	{
		return new Model<>("");
	}
}
