package nl.rivm.screenit.main.web.gebruiker.clienten.contact.gen;

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

import java.util.List;

import nl.rivm.screenit.main.service.algemeen.OverdrachtPersoonsgegevensService;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ClientContactInzagePersoonsgegevensPanel extends AbstractClientContactActiePanel<ClientContactActie>
{
	private static final long serialVersionUID = 1L;

	private IModel<Client> clientModel;

	@SpringBean
	private OverdrachtPersoonsgegevensService overdrachtPersoonsgegevensService;

	public ClientContactInzagePersoonsgegevensPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);
		clientModel = client;
	}

	@Override
	public void validate()
	{
		if (overdrachtPersoonsgegevensService.heeftVerzoekZonderGegenereerdeBrief(clientModel.getObject()))
		{
			error(getString("message.aanvraagbriefOnderweg"));
		}
	}

	@Override
	protected void onDetach()
	{
		ModelUtil.nullSafeDetach(clientModel);
		super.onDetach();
	}
}
