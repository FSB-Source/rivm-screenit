
package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.selectie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.annotation.Nullable;

import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.enums.SelectieType;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportage;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportageEntry;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportageGewijzigdGebiedEntry;
import nl.rivm.screenit.model.verwerkingverslag.SelectieRapportageProjectGroepEntry;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.springframework.beans.support.PropertyComparator;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;

public class SelectieVerslagPanel extends GenericPanel<SelectieRapportage>
{

	private static final long serialVersionUID = 1L;

	public SelectieVerslagPanel(String id, final IModel<SelectieRapportage> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("datumVerwerking", "dd-MM-yyyy HH:mm:ss"));

		add(new PropertyListView<SelectieRapportageEntry>("gemaakt", new IModel<List<SelectieRapportageEntry>>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public List<SelectieRapportageEntry> getObject()
			{
				List<SelectieRapportageEntry> selectieRapportageEntries = model.getObject().getEntries();

				selectieRapportageEntries = new ArrayList<>(Collections2.filter(selectieRapportageEntries, new Predicate<SelectieRapportageEntry>()
				{
					@Override
					public boolean apply(@Nullable SelectieRapportageEntry input)
					{
						return input.getSelectieType() == SelectieType.UITNODIGING_GEMAAKT;
					}
				}));
				Collections.sort(selectieRapportageEntries, new Comparator<SelectieRapportageEntry>()
				{

					@Override
					public int compare(SelectieRapportageEntry o1, SelectieRapportageEntry o2)
					{
						return o1.getColonUitnodigingCategorie().name().compareTo(o2.getColonUitnodigingCategorie().name());
					}
				});
				return selectieRapportageEntries;
			}
		})
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<SelectieRapportageEntry> item)
			{
				SelectieRapportageEntry entry = item.getModelObject();
				item.add(new EnumLabel<ColonUitnodigingCategorie>("colonUitnodigingCategorie"));
				item.add(new Label("aantal"));
				item.add(new Label("gepusht", getPushString(entry)));
			}
		});

		add(new PropertyListView<SelectieRapportageProjectGroepEntry>("groep", new IModel<List<SelectieRapportageProjectGroepEntry>>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public List<SelectieRapportageProjectGroepEntry> getObject()
			{
				List<SelectieRapportageProjectGroepEntry> selectieRapportageEntries = model.getObject().getProjectGroepen();

				selectieRapportageEntries = new ArrayList<>(Collections2.filter(selectieRapportageEntries, new Predicate<SelectieRapportageProjectGroepEntry>()
				{
					@Override
					public boolean apply(@Nullable SelectieRapportageProjectGroepEntry input)
					{
						return input.getSelectieType() == SelectieType.UITNODIGING_GEMAAKT;
					}
				}));
				Collections.sort(selectieRapportageEntries, new Comparator<SelectieRapportageProjectGroepEntry>()
				{

					@Override
					public int compare(SelectieRapportageProjectGroepEntry o1, SelectieRapportageProjectGroepEntry o2)
					{
						String projectGroep1 = o1.getProjectGroep().getProject().getNaam() + "/" + o1.getProjectGroep().getNaam();
						String projectGroep2 = o2.getProjectGroep().getProject().getNaam() + "/" + o2.getProjectGroep().getNaam();
						return projectGroep1.compareTo(projectGroep2);
					}
				});
				return selectieRapportageEntries;
			}
		})
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<SelectieRapportageProjectGroepEntry> item)
			{
				SelectieRapportageProjectGroepEntry groepEntry = item.getModelObject();
				if (groepEntry.getDagenNogTeGaan() == 0 && item.getModelObject().getClientenNogTeGaan() > 0)
				{
					item.add(new AttributeAppender("class", Model.of("error")));
				}
				item.add(new Label("dagenNogTeGaan"));
				item.add(new Label("clientenNogTeGaan"));
				item.add(new Label("projectGroep.naam"));
				item.add(new Label("projectGroep.project.naam"));
				item.add(new Label("aantal"));
				if (item.getModelObject().getWaarvanGepusht() > 0)
				{
					item.add(new Label("gepusht", String.format("(waarvan gepusht: %s)", item.getModelObject().getWaarvanGepusht())));
				}
				else
				{
					item.add(new EmptyPanel("gepusht"));
				}
			}
		});

		List<SelectieRapportageGewijzigdGebiedEntry> list = new ArrayList<>(model.getObject().getGewijzigdeGebieden());
		Collections.sort(list, new PropertyComparator("uitnodigingsGebied.naam", false, true));
		add(new ListView<SelectieRapportageGewijzigdGebiedEntry>("gewijzigdeGebieden", ModelUtil.listRModel(list, false))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<SelectieRapportageGewijzigdGebiedEntry> item)
			{
				item.setModel(new CompoundPropertyModel<SelectieRapportageGewijzigdGebiedEntry>(item.getModel()));
				item.add(new Label("uitnodigingsGebied.naam"));
				if (Integer.valueOf(Integer.MAX_VALUE).equals(item.getModelObject().getPercentage()))
				{

					item.add(new Label("percentage", "&#8734;").setEscapeModelStrings(false));
				}
				else
				{
					item.add(new Label("percentage"));
				}
			}
		});
	}

	private static String getPushString(SelectieRapportageEntry entry)
	{
		String tekst = "";
		if (entry.getWaarvanGepusht() != null && entry.getWaarvanGepusht() > 0)
		{
			tekst = "(waarvan gepusht: " + entry.getWaarvanGepusht() + ")";
		}
		return tekst;
	}
}
