package nl.rivm.screenit.main.web.gebruiker.clienten.complicatie;

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
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPage;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.model.IModel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_CLIENT_COMPLICATIE_REGISTREREN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ClientComplicatiePage extends ClientPage
{

	private static final long serialVersionUID = 1L;

	private IModel<Client> clientModel;

	public ClientComplicatiePage(IModel<Client> client)
	{
		super(client);
		clientModel = client;

		add(new ClientPaspoortPanel("passpoort", client));

		add(new ClientComplicatieOverzichtPanel("complicaties", client));

		IndicatingAjaxLink<Void> link = new IndicatingAjaxLink<Void>("toevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ClientComplicatieEditPage(getClientModel(), null));
			}

		};
		link.setVisible(ScreenitSession.get().getToegangsLevel(Actie.TOEVOEGEN, Recht.GEBRUIKER_CLIENT_COMPLICATIE_REGISTREREN) != null);
		add(link);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(clientModel);
	}

	public IModel<Client> getClientModel()
	{
		return clientModel;
	}

	public void setClientModel(IModel<Client> clientModel)
	{
		this.clientModel = clientModel;
	}
}
