package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.brievengenereren;

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

import java.util.List;

import nl.rivm.screenit.model.verwerkingverslag.BrievenGenererenRapportage;
import nl.rivm.screenit.model.verwerkingverslag.BrievenGenererenRapportageEntry;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;

public class BrievenGenererenVerslagPanel extends GenericPanel<BrievenGenererenRapportage>
{

	private static final long serialVersionUID = 1L;

	public BrievenGenererenVerslagPanel(String id, IModel<BrievenGenererenRapportage> model)
	{
		super(id, new CompoundPropertyModel<>(model));

		add(DateLabel.forDatePattern("datumVerwerking", "dd-MM-yyyy HH:mm:ss"));

		List<BrievenGenererenRapportageEntry> entries = model.getObject().getEntries();

		add(new Label("AantalTotaalBrieven", getAantalTotaalBrieven(entries)));
		add(new Label("aantalTotaalScreeningOrganisaties", getAantalTotaalScreeningOrganisaties(entries)));

		add(new PropertyListView<BrievenGenererenRapportageEntry>("verwerkingen", ModelUtil.listRModel(entries, false))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<BrievenGenererenRapportageEntry> item)
			{
				item.add(new Label("screeningOrganisatie", item.getModelObject().getScreeningOrganisatie().getNaam()));
				item.add(new Label("aantalBrievenPerScreeningOrganisatie"));
			}

		});
	}

	private Long getAantalTotaalBrieven(List<BrievenGenererenRapportageEntry> verwerkingen)
	{
		if (verwerkingen.isEmpty())
		{
			return (long) 0;
		}
		else
		{
			Long restulaten = (long) 0;
			for (BrievenGenererenRapportageEntry entry : verwerkingen)
			{
				restulaten += entry.getAantalBrievenPerScreeningOrganisatie();
			}
			return restulaten;
		}

	}

	private Integer getAantalTotaalScreeningOrganisaties(List<BrievenGenererenRapportageEntry> verwerkingen)
	{
		if (verwerkingen.isEmpty())
		{
			return 0;
		}
		else
		{
			return verwerkingen.size();
		}
	}

}
