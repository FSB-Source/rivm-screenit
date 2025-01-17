package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.opschorting;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientContactActieTypeWrapper;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.AbstractMammaCePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IModel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_CENTRALE_EENHEID_OPSCHORTEN_BEOORDELINGEN },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaClientContactNaOpgeschortOnderzoekPage extends AbstractMammaCePage
{

	public MammaClientContactNaOpgeschortOnderzoekPage(IModel<Client> clientModel, List<Object> extraParameters, ClientContactActieTypeWrapper... defaultSelectedActies)
	{
		setDefaultModel(clientModel);
		add(new ClientContactPanel("panel", clientModel, extraParameters, defaultSelectedActies)
		{
			@Override
			protected void contactAfgerond()
			{
				setResponsePage(new MammaCeOpgeschorteBeoordelingenWerklijstPage());
			}

			@Override
			protected void contactNietAfgerond(Client client)
			{
				List<Object> extraParameters = new ArrayList<>();
				extraParameters.add(Constants.CONTACT_EXTRA_PARAMETER_VANUIT_BK_PLANNING);
				extraParameters.add(Constants.CONTACT_EXTRA_PARAMETER_ALLEEN_CLIENT_CONTACT);
				setResponsePage(new MammaClientContactNaOpgeschortOnderzoekPage(ModelUtil.sModel(client), extraParameters, defaultSelectedActies));
			}
		});
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = super.getContextMenuItems();
		contextMenuItems
			.add(new GebruikerMenuItem("label.tab.mammascreening.ce-onderbroken-onderzoeken-werklijst-client-contact", false, MammaClientContactNaOpgeschortOnderzoekPage.class));

		return contextMenuItems;
	}

}
