package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.centraleeenheid;

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

import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class GekoppeldeBeEnSePanel extends GenericPanel<CentraleEenheid>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	private IModel<List<BeoordelingsEenheid>> beoordelingseenheden;

	GekoppeldeBeEnSePanel(String id, IModel<CentraleEenheid> model)
	{
		super(id, model);

		beoordelingseenheden = ModelUtil.listRModel(instellingService.getChildrenInstellingen(getModelObject(), BeoordelingsEenheid.class));

		add(new ListView<BeoordelingsEenheid>("beLijst", beoordelingseenheden)
		{
			protected void populateItem(ListItem<BeoordelingsEenheid> item)
			{
				BeoordelingsEenheid beoordelingsEenheid = item.getModelObject();
				item.add(new Label("beoordelingseenheid", beoordelingsEenheid.getNaam()));
				item.add(new Label("seLijst", screeningsEenheidService.getScreeningsEenhedenNamen(beoordelingsEenheid)));
			}
		});
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(beoordelingseenheden);
	}
}
