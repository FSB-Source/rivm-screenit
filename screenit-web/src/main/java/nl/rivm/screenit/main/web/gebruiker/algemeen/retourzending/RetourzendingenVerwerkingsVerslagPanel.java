
package nl.rivm.screenit.main.web.gebruiker.algemeen.retourzending;

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

import java.util.Arrays;

import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.model.enums.RetourzendingAfhandelingType;
import nl.rivm.screenit.model.logging.RetourzendingLogEvent;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;

public class RetourzendingenVerwerkingsVerslagPanel extends GenericPanel<RetourzendingLogEvent>
{

	private static final long serialVersionUID = 1L;

	public RetourzendingenVerwerkingsVerslagPanel(String id, IModel<RetourzendingLogEvent> model)
	{
		super(id, model);

		ListView<RetourzendingAfhandelingType> afhandelingLogListView = new ListView<RetourzendingAfhandelingType>("afhandelingLogListView",
			Arrays.asList(RetourzendingAfhandelingType.values()))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<RetourzendingAfhandelingType> item)
			{
				RetourzendingAfhandelingType value = item.getModelObject();
				item.add(new Label("label", new SimpleStringResourceModel(EnumStringUtil.getPropertyString(value))));
				item.add(new Label("aantal", new PropertyModel<Integer>(RetourzendingenVerwerkingsVerslagPanel.this.getModel(), value.getProperty())));
			}

		};

		add(afhandelingLogListView);

		add(new Label("trackIdMelding"));

		add(new Label("retourRedenMelding"));

		add(new Label("clientNietGevondenMelding"));

		add(new Label("zendingHeeftAlRetourstatusMelding"));

		add(new Label("rondeNietActiefMelding"));

		add(new Label("uitnodigingNietValideMelding"));

		add(new Label("skippedRegels"));
	}
}
