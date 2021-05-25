package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.mamma.ilm;

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

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.logging.MammaIlmLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIlmBeeldenStatusRapportageEntry;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class MammaIlmBeeldenStatusRapportagePanel extends GenericPanel<MammaIlmLogEvent>
{
	public MammaIlmBeeldenStatusRapportagePanel(String id, IModel<MammaIlmLogEvent> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("logRegel.gebeurtenisDatum", "dd-MM-yyyy HH:mm:ss"));
		add(new Label("rapportage.aantalBeelden"));

		List<MammaIlmBeeldenStatusRapportageEntry> entries = getModelObject().getRapportage().getEntries()
			.stream()
			.filter(v -> v.getStatusDatum() != null)
			.sorted(Comparator.comparing(MammaIlmBeeldenStatusRapportageEntry::getStatusDatum))
			.collect(Collectors.toList());

		add(new PropertyListView<MammaIlmBeeldenStatusRapportageEntry>("entries", ModelUtil.listRModel(entries, false))
		{
			@Override
			protected void populateItem(ListItem<MammaIlmBeeldenStatusRapportageEntry> entry)
			{
				entry.add(new Label("uitnodigingsNr"));
				entry.add(DateLabel.forDatePattern("statusDatum", "dd-MM-yyyy HH:mm:ss"));
			}
		});
	}
}
