
package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.cervix.zas;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.main.web.component.BriefTypeLabel;
import nl.rivm.screenit.model.logging.CervixUitnodigingVersturenLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixZasVersturenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixZasVersturenRapportageProjectEntry;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class ZasVersturenVerslagPanel extends GenericPanel<CervixUitnodigingVersturenLogEvent>
{

	private static final long serialVersionUID = 1L;

	public ZasVersturenVerslagPanel(String id, IModel<CervixUitnodigingVersturenLogEvent> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("logRegel.gebeurtenisDatum", "dd-MM-yyyy HH:mm:ss"));

		add(new PropertyListView<CervixZasVersturenRapportage>("verstuurd", new IModel<List<CervixZasVersturenRapportage>>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public List<CervixZasVersturenRapportage> getObject()
			{
				List<CervixZasVersturenRapportage> selectieRapportageEntries = model.getObject().getRapportage();

				selectieRapportageEntries = new ArrayList<>(selectieRapportageEntries);
				Collections.sort(selectieRapportageEntries, new Comparator<CervixZasVersturenRapportage>()
				{

					@Override
					public int compare(CervixZasVersturenRapportage o1, CervixZasVersturenRapportage o2)
					{
						return o1.getBriefType().name().compareTo(o2.getBriefType().name());
					}
				});
				return selectieRapportageEntries;
			}
		})
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<CervixZasVersturenRapportage> item)
			{
				item.setModel(new CompoundPropertyModel<>(item.getModel()));
				item.add(new BriefTypeLabel("briefType"));
				item.add(new Label("aantal"));
			}
		});

		add(new PropertyListView<CervixZasVersturenRapportageProjectEntry>("groep", new IModel<List<CervixZasVersturenRapportageProjectEntry>>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public List<CervixZasVersturenRapportageProjectEntry> getObject()
			{
				List<CervixZasVersturenRapportageProjectEntry> selectieRapportageEntries = model.getObject().getProjectGroepen();
				selectieRapportageEntries = new ArrayList<>(selectieRapportageEntries);

				Collections.sort(selectieRapportageEntries, new Comparator<CervixZasVersturenRapportageProjectEntry>()
				{

					@Override
					public int compare(CervixZasVersturenRapportageProjectEntry o1, CervixZasVersturenRapportageProjectEntry o2)
					{
						return o1.getProjectGroep().getProject().getNaam().compareTo(o2.getProjectGroep().getProject().getNaam());
					}
				});
				return selectieRapportageEntries;
			}
		})
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<CervixZasVersturenRapportageProjectEntry> item)
			{
				item.add(new Label("projectGroep.project.naam"));
				item.add(new Label("projectGroep.naam"));
				item.add(new Label("aantal"));
			}
		});

	}
}
