package nl.rivm.screenit.main.web.client.dashboard.colon;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.web.client.dashboard.ClientBaseBezwaarPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

import org.apache.wicket.model.IModel;

public class ClientColonBezwaarPanel extends ClientBaseBezwaarPanel
{
	private static final long serialVersionUID = 1L;

	public ClientColonBezwaarPanel(String id, IModel<ClientContactActie> model, IModel<Client> clientModel, List<Object> extraPanelParams)
	{
		super(id, model, clientModel, Arrays.asList(Bevolkingsonderzoek.COLON));
	}

}
