package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientContactActieTypeWrapper;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class MammaNaarAfspraakMakenPanel extends GenericPanel<Client>
{
	public MammaNaarAfspraakMakenPanel(String id, IModel<Client> clientModel)
	{
		super(id, clientModel);
		IndicatingAjaxLink<Client> link = new IndicatingAjaxLink<Client>("aanmaken")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				List<Object> extraParameters = new ArrayList<>();
				extraParameters.add(Constants.CONTACT_EXTRA_PARAMETER_VANUIT_BK_PLANNING);

				Client client = MammaNaarAfspraakMakenPanel.this.getModelObject();
				MammaScreeningRonde ronde = client.getMammaDossier().getLaatsteScreeningRonde();
				ClientContactActieTypeWrapper actie = ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN;
				if (ronde != null && ronde.getLaatsteUitnodiging().getLaatsteAfspraak() != null
					&& !MammaAfspraakStatus.isGeannuleerd(ronde.getLaatsteUitnodiging().getLaatsteAfspraak().getStatus()))
				{
					actie = ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_WIJZIGEN;
					extraParameters.add(MammaAfspraakStatus.GEPLAND);
					extraParameters.add(ronde.getLaatsteUitnodiging().getLaatsteAfspraak());
				}
				setResponsePage(
					new MammaAfspraakVerzettenAfmeldenPage(MammaNaarAfspraakMakenPanel.this.getModel(), extraParameters, actie));
			}

		};
		add(link);
		link.add(new AttributeAppender("class", Model.of("no-event-bubble-up icon-share-alt")));

	}

}
