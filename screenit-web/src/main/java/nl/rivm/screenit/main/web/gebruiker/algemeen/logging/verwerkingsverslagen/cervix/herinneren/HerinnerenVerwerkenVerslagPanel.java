package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.cervix.herinneren;

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

import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixHerinnerenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixHerinnerenRapportageBriefType;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class HerinnerenVerwerkenVerslagPanel extends GenericPanel<CervixHerinnerenRapportage>
{

	private static final long serialVersionUID = 1L;

	public HerinnerenVerwerkenVerslagPanel(String id, IModel<CervixHerinnerenRapportage> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("datumVerwerking", "dd-MM-yyyy HH:mm:ss"));
		add(new Label("totaalAantal"));

		add(new PropertyListView<CervixHerinnerenRapportageBriefType>("briefTypen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<CervixHerinnerenRapportageBriefType> entry)
			{
				entry.add(new Label("briefType.weergaveNaam"));
				entry.add(new Label("aantal"));
			}
		});
	}
}
