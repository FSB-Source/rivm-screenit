package nl.rivm.screenit.main.web.component.bezwaar.edit;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.algemeen.BezwaarViewWrapper;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.service.BezwaarService;

import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class BezwaarEditPanel extends Panel
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private BezwaarService bezwaarService;

	public BezwaarEditPanel(String id, List<BezwaarGroupViewWrapper> wrappers, boolean magVerwijderenVanDossierGetoondWorden)
	{
		super(id);

		final int span = bepaalSpan(wrappers);
		ListView<BezwaarGroupViewWrapper> listView = new ListView<BezwaarGroupViewWrapper>("listView", wrappers)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<BezwaarGroupViewWrapper> item)
			{
				BezwaarGroupViewWrapper wrapper = item.getModelObject();

				WebMarkupContainer container = new WebMarkupContainer("container");
				if ("ALGEMEEN".equals(wrapper.getKey()))
				{
					container.add(new AttributeAppender("class", "span12"));
				}
				else
				{
					container.add(new AttributeAppender("class", "span" + span));
				}

				container.add(new Label("bvo", getString("Bevolkingsonderzoek." + wrapper.getKey())));

				ListView<BezwaarViewWrapper> bezwaren = new ListView<BezwaarViewWrapper>("bezwaren", wrapper.getBezwaren())
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void populateItem(ListItem<BezwaarViewWrapper> item)
					{

						BezwaarType type = item.getModelObject().getType();
						if (!BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER.equals(type)
							|| (magVerwijderenVanDossierGetoondWorden && BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER.equals(type)))
						{
							item.add(new BezwaarRadioChoice("bezwaarRadioChoice", item.getModel()));
						}
						else
						{
							item.add(new EmptyPanel("bezwaarRadioChoice"));
						}
					}

				};
				container.add(bezwaren);
				item.add(container);
			}
		};
		add(listView);
	}

	private static int bepaalSpan(List<BezwaarGroupViewWrapper> wrappers)
	{
		int span = 12; 
		if (wrappers.size() == 3)
		{
			span = 6; 
		}
		else if (wrappers.size() == 4)
		{
			span = 4; 
		}
		return span;
	}
}
