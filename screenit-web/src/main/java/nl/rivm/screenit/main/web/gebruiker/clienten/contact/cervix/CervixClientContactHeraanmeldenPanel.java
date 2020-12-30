package nl.rivm.screenit.main.web.gebruiker.clienten.contact.cervix;

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

import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactHeraanmeldenPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;

import org.apache.wicket.model.IModel;

public class CervixClientContactHeraanmeldenPanel extends AbstractClientContactHeraanmeldenPanel<CervixDossier, CervixAfmelding, CervixBrief, CervixScreeningRonde>
{
	private static final long serialVersionUID = 1L;

	public CervixClientContactHeraanmeldenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model, client);
	}

	@Override
	protected CervixDossier getDossier(Client client)
	{
		return client.getCervixDossier();
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> opslaanMeldingen = super.getOpslaanMeldingen();
		opslaanMeldingen.add("Er wordt automatisch een uitnodiging gestuurd");
		return opslaanMeldingen;
	}
}
