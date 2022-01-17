package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.cervix.huisartsberichten;

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

import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixHuisartsberichtenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixHuisartsberichtenRapportageEntry;

import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;

public class HuisartsberichtenVerslagPanel extends GenericPanel<CervixHuisartsberichtenRapportage>
{

	private static final long serialVersionUID = 1L;

	public HuisartsberichtenVerslagPanel(String id, IModel<CervixHuisartsberichtenRapportage> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("datumVerwerking", "dd-MM-yyyy HH:mm:ss"));
		add(new Label("aantalHuisartsBepaald"));
		add(new Label("aantalHuisartsKonNietWordenBepaald"));
		add(new Label("aantalHuisartsOnbekend"));
		add(new Label("aantalVerstuud"));
		add(new Label("aantalVersturenMislukt"));

		add(new PropertyListView<CervixHuisartsberichtenRapportageEntry>("entries")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<CervixHuisartsberichtenRapportageEntry> entry)
			{
				entry.add(new Label("huisartsBerichtType", entry.getModelObject().getHuisartsBerichtType().getNaam()));
				entry.add(new Label("aantalVerstuurd"));
				entry.add(new Label("aantalVersturenMislukt"));
			}
		});
	}
}
