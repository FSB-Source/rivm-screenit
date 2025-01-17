package nl.rivm.screenit.main.web.component.bezwaar.tekst;

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

import java.util.List;

import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.algemeen.BezwaarViewWrapper;
import nl.rivm.screenit.service.BezwaarService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class BezwaarTekstPanel extends GenericPanel<BezwaarMoment>
{

	@SpringBean
	private BezwaarService bezwaarService;

	public BezwaarTekstPanel(String id, IModel<BezwaarMoment> model)
	{
		this(id, model, true);
	}

	public BezwaarTekstPanel(String id, IModel<BezwaarMoment> model, boolean verwijderDossierZichtBaar)
	{
		super(id, ModelUtil.sModel(model.getObject()));
		BezwaarMoment moment = model.getObject();

		List<BezwaarGroupViewWrapper> wrappers = bezwaarService.getBezwaarGroupViewWrappers(moment, verwijderDossierZichtBaar);

		ListView<BezwaarGroupViewWrapper> listView = new ListView<BezwaarGroupViewWrapper>("listView", wrappers)
		{
			@Override
			protected void populateItem(ListItem<BezwaarGroupViewWrapper> item)
			{
				final BezwaarGroupViewWrapper entry = item.getModelObject();
				item.add(new Label("bvo", getString("Bevolkingsonderzoek." + entry.getKey())));
				ListView<BezwaarViewWrapper> bezwaren = new ListView<BezwaarViewWrapper>("bezwaren", entry.getBezwaren())
				{

					@Override
					protected void populateItem(ListItem<BezwaarViewWrapper> item)
					{
						BezwaarViewWrapper wrapper = item.getModelObject();
						item.add(new Label("bezwaar", new SimpleStringResourceModel(wrapper.getResourceKey())));
					}
				};
				item.add(bezwaren);
			}
		};
		add(listView);
	}
}
