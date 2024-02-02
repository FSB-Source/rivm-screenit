package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactHeraanmeldenPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;

import org.apache.wicket.model.IModel;

public class ColonClientContactHeraanmeldenPanel extends AbstractClientContactHeraanmeldenPanel<ColonDossier, ColonAfmelding, ColonBrief, ColonScreeningRonde>
{

	private static final long serialVersionUID = 1L;

	public ColonClientContactHeraanmeldenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model, client);
	}

	@Override
	protected ColonDossier getDossier(Client client)
	{
		return client.getColonDossier();
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		if (afspraakMakenPanel != null)
		{
			return afspraakMakenPanel.getOpslaanMeldingen();
		}
		return new ArrayList<>();
	}

}
