
package nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren;

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

import java.util.Map;

import nl.topicuszorg.formulieren2.api.instantie.LabelInstantie;
import nl.topicuszorg.formulieren2.wicketrenderer.HerhalingHelper;

import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;

public class RomanHerhalingLabelPanel extends Panel
{

	private static final long serialVersionUID = 1L;

	public RomanHerhalingLabelPanel(String id, final IModel<LabelInstantie> model)
	{
		super(id, model);
		add(new Label("label", new IModel<String>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				Map<Integer, Integer> herhalingen = HerhalingHelper.bepaalHerhalingen(RomanHerhalingLabelPanel.this);

				Integer herhaling = null;
				if (herhalingen != null && !herhalingen.isEmpty())
				{
					herhaling = herhalingen.get(herhalingen.size() - 1);
				}
				return model.getObject().getLabelTekst() + " " + (herhaling + 1);
			}
		})
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onComponentTag(ComponentTag tag)
			{
				if (model.getObject().getHeading() != null)
				{
					tag.setName("h" + model.getObject().getHeading());
				}
				super.onComponentTag(tag);
			}

		});
	}
}
