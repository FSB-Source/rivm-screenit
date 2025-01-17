package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

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

import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followupradiologie.MammaFollowUpRadiologieVerslagInzienPanel;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class MammaBeFollowUpRadiologiePanel extends AbstractBEAccordionPanel<MammaBeoordeling>
{
	final private IModel<List<MammaFollowUpRadiologieVerslag>> followUpVerslagen;

	public MammaBeFollowUpRadiologiePanel(String id, IModel<MammaBeoordeling> model, List<MammaFollowUpRadiologieVerslag> followUpVerslagen)
	{
		super(id, model, Model.of("Follow Up Radiologie"), 6);
		super.setIngeklapt(true);
		this.followUpVerslagen = ModelUtil.listRModel(followUpVerslagen, false);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		ListView<MammaFollowUpRadiologieVerslag> followUpVerslagList = new ListView<MammaFollowUpRadiologieVerslag>("followUpRadiologieVerslagen", followUpVerslagen)
		{
			@Override
			protected void populateItem(ListItem<MammaFollowUpRadiologieVerslag> followUpVerslagListItem)
			{
				followUpVerslagListItem.add(new MammaFollowUpRadiologieVerslagInzienPanel("radiologieVerslag", followUpVerslagListItem.getModel()));
			}
		};
		panelContainer.add(followUpVerslagList);
	}
}
