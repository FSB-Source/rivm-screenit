package nl.rivm.screenit.main.web.gebruiker.screening.colon.overeenkomstenzoeken;

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

import nl.rivm.screenit.model.overeenkomsten.AfgeslotenInstellingOvereenkomst;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;

public class AfgeslotenOvereenkomstenLijstPanel extends Panel
{

	private static final long serialVersionUID = 1L;

	public AfgeslotenOvereenkomstenLijstPanel(String id, IModel<List<AfgeslotenInstellingOvereenkomst>> model)
	{
		super(id, model);
		add(new ListView<AfgeslotenInstellingOvereenkomst>("lijst", model)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<AfgeslotenInstellingOvereenkomst> item)
			{
				AfgeslotenInstellingOvereenkomst overeenkomst = (AfgeslotenInstellingOvereenkomst) item.getDefaultModelObject();
				StringBuilder sb = new StringBuilder();
				sb.append(overeenkomst.getOvereenkomst().getNaam());
				sb.append(" (");
				if (overeenkomst.getStartDatum() != null)
				{
					sb.append(overeenkomst.getStartDatum());
					sb.append(", ");
				}
				else
				{
					sb.append("onbekend, ");
				}
				if (overeenkomst.getEindDatum() != null)
				{
					sb.append(overeenkomst.getEindDatum());
				}
				else
				{
					sb.append("onbekend");
				}
				sb.append(")");
				item.add(new Label("tekst", sb.toString()));
			}
		});
	}

}
