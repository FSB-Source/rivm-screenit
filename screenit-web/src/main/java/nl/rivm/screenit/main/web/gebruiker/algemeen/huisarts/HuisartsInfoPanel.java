package nl.rivm.screenit.main.web.gebruiker.algemeen.huisarts;

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

import nl.rivm.screenit.main.model.BaseHuisartsModel;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;

public class HuisartsInfoPanel extends GenericPanel<BaseHuisartsModel<?>>
{
	public HuisartsInfoPanel(String id, BaseHuisartsModel<?> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(new Label("huisartsNaam"));
		add(new Label("praktijkNaam"));
		add(new Label("praktijkAdres"));
		add(new Label("huisartsAgb"));
		add(new Label("weergaveNaam"));
		add(new Label("klantnummer"));
		add(new Label("ediadres"));
		add(new Label("communicatieadres"));
		add(new WebMarkupContainer("isVerwijderd").setVisible(model.isVerwijderd()));
	}

}
