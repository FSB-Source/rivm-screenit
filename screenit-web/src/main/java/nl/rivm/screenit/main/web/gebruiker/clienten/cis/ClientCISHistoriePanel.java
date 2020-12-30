package nl.rivm.screenit.main.web.gebruiker.clienten.cis;

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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import nl.rivm.screenit.main.util.CervixCisHistoryUtil;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorieOngestructureerdRegel;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;

import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;

public class ClientCISHistoriePanel extends GenericPanel<Client>
{

	private List<String> rondeList = new ArrayList<>();

	private Map<String, IModel<List<CervixCISHistorieOngestructureerdRegel>>> regelsPerRonde = new TreeMap<>();

	private final ListView<String> listView;

	public ClientCISHistoriePanel(String id, IModel<Client> client)
	{
		super(id, client);

		if (client.getObject().getCervixDossier() != null && client.getObject().getCervixDossier().getCisHistorie() != null)
		{
			Map<String, List<CervixCISHistorieOngestructureerdRegel>> regelsPerRonde = CervixCisHistoryUtil.getOngestructureerdeRegelsPerRonde(
				client.getObject().getCervixDossier().getCisHistorie(), false);
			rondeList = CervixCisHistoryUtil.getOrderdKeys(regelsPerRonde, false);
			for (Entry<String, List<CervixCISHistorieOngestructureerdRegel>> ronde : regelsPerRonde.entrySet())
			{
				this.regelsPerRonde.put(ronde.getKey(), new SimpleListHibernateModel<>(ronde.getValue()));
			}
		}
		listView = getCISRondeOverzicht();
		add(listView);
	}

	private ListView<String> getCISRondeOverzicht()
	{
		ListView<String> listView = new ListView<String>("rondes", Model.ofList(this.rondeList))
		{
			@Override
			protected void populateItem(ListItem<String> item)
			{
				item.add(new Label("index", item.getModelObject()));
				getGebeurtenissenListView(item, item.getModelObject());
			}
		};
		listView.setOutputMarkupId(true);
		return listView;
	}

	private void getGebeurtenissenListView(ListItem<String> item, String ronde)
	{
		item.add(new PropertyListView<CervixCISHistorieOngestructureerdRegel>("gebeurtenissen", getGebeurtenissenVoorRonde(ronde))
		{
			@Override
			protected void populateItem(ListItem<CervixCISHistorieOngestructureerdRegel> item)
			{
				item.add(new Label("tekst", item.getModel().getObject().getTekst()));
				item.add(DateLabel.forDatePattern("datum", "dd-MM-yyyy HH:mm:ss"));
			}
		});
	}

	private IModel<List<CervixCISHistorieOngestructureerdRegel>> getGebeurtenissenVoorRonde(String ronde)
	{
		if (regelsPerRonde.containsKey(ronde))
		{
			return regelsPerRonde.get(ronde);
		}
		return new ListModel<>(new ArrayList<>());
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		for (IModel<List<CervixCISHistorieOngestructureerdRegel>> model : regelsPerRonde.values())
		{
			model.detach();
		}
	}
}
