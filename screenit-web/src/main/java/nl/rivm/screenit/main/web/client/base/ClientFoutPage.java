package nl.rivm.screenit.main.web.client.base;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.client.dashboard.home.ClientHomeDashboardPage;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.util.FoutmeldingsCodeUtil;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ClientFoutPage extends ClientBasePage
{
	private static final Logger LOG = LoggerFactory.getLogger(ClientFoutPage.class);

	private static final long serialVersionUID = 1L;

	@SpringBean(name = "applicatieInstantie")
	private String applicatieInstance;

	public <C extends BasePage> ClientFoutPage()
	{
		this(null, ClientHomeDashboardPage.class, true);
	}

	public <C extends BasePage> ClientFoutPage(String melding, final Class<C> gaTerugPage)
	{
		this(melding, gaTerugPage, false);
	}

	public <C extends BasePage> ClientFoutPage(String melding, final Class<C> gaTerugPage, boolean crash)
	{
		if (melding == null)
		{
			melding = getString("onbekende.fout");
		}
		if (crash)
		{
			createCrashMelding();
		}
		else
		{
			add(new EmptyPanel("errorId").setVisible(false));
		}
		add(new Label("dashboardMelding", melding));
		add(new Link<Void>("terug")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(gaTerugPage);
			}
		});
	}

	private void createCrashMelding()
	{
		String errorId = FoutmeldingsCodeUtil.getFoutmeldingsCode("CP");

		String gebruikerId = "";
		Account loggedInAccount = ScreenitSession.get().getLoggedInAccount();
		if (loggedInAccount != null)
		{
			if (loggedInAccount instanceof InstellingGebruiker)
			{
				gebruikerId = "IG";
			}
			else if (loggedInAccount instanceof Gebruiker)
			{
				gebruikerId = "G";
			}
			else if (loggedInAccount instanceof Client)
			{
				gebruikerId = "C";
			}
			gebruikerId += loggedInAccount.getId().toString();
		}

		LOG.error(String.format("Fout in applicatie (ErrorCode: %s, gebruiker: %s)", errorId, gebruikerId));
		add(new Label("errorId", applicatieInstance + "_" + errorId));
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
