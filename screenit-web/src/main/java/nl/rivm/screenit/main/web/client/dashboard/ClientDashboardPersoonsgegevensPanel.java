package nl.rivm.screenit.main.web.client.dashboard;

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

import nl.rivm.screenit.main.web.client.base.ClientHoofdMenuitem;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.model.Client;

import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public class ClientDashboardPersoonsgegevensPanel extends GenericPanel<Client>
{

	public ClientDashboardPersoonsgegevensPanel(String id, IModel<Client> clientModel, ClientHoofdMenuitem menu)
	{
		super(id);

		add(new ClientGegevensPanel("clientGegevens", new SimpleStringResourceModel("label.gegevens"), clientModel, menu));
		add(new ContactGegevensPanel("contactGegevens", new SimpleStringResourceModel("label.contactgegevens"), clientModel, menu));
		add(new ClientOverdrachtPersoonsgegevensPanel("gegevensoverdracht", new SimpleStringResourceModel("label.gegevensoverdracht"), clientModel, menu).setVisible(false));

	}
}
