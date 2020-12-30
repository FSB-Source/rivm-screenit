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

import java.util.Collections;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.service.ClientGebeurtenis;
import nl.rivm.screenit.main.util.EnumStringUtil;
import nl.rivm.screenit.model.Client;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public abstract class ClientHistoryPanel extends GenericPanel<Client>
{
	public ClientHistoryPanel(String id, IModel<Client> modelClient)
	{
		super(id);

		List<ClientGebeurtenis> gebeurtenissen = getGebeurtenissen(modelClient);
		Collections.sort(gebeurtenissen);
		ListView<ClientGebeurtenis> list = new ListView<ClientGebeurtenis>("history", gebeurtenissen)
		{
			@Override
			protected void populateItem(ListItem<ClientGebeurtenis> item)
			{
				item.add(DateLabel.forDatePattern("datum", new PropertyModel<Date>(item.getModel(), "datum"), "dd-MM-yyyy HH:mm"));
				String[] extraOmschrijvingen = item.getModelObject().getExtraParam();
				if (extraOmschrijvingen != null)
				{
					int i = 0;
					for (String omschrijving : extraOmschrijvingen)
					{
						if (omschrijving != null)
						{
							extraOmschrijvingen[i++] = getString(omschrijving, null, omschrijving);
						}
						else
						{
							extraOmschrijvingen[i++] = "";
						}
					}
				}
				item.add(new Label("tekst", String.format(getString(EnumStringUtil.getPropertyString(item.getModelObject().getType())), (Object[]) extraOmschrijvingen)));

			}
		};
		list.setVisible(gebeurtenissen.size() > 0);
		add(list);

	}

	protected abstract List<ClientGebeurtenis> getGebeurtenissen(IModel<Client> modelClient);
}
