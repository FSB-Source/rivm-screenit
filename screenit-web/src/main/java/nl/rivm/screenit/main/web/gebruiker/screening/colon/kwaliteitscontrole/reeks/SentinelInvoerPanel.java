package nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole.reeks;

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

import nl.rivm.screenit.model.colon.SKMLSentineelControleBarcode;
import nl.rivm.screenit.model.colon.enums.SKMLSentineelControleType;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;

public class SentinelInvoerPanel extends Panel
{
	public SentinelInvoerPanel(String id, IModel<List<SKMLSentineelControleBarcode>> allSentineelsModels, String set)
	{
		super(id, allSentineelsModels);

		add(new Label("set", set));

		ListView<SKMLSentineelControleBarcode> sentineelList = new ListView<SKMLSentineelControleBarcode>("sentineelLijst", allSentineelsModels)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<SKMLSentineelControleBarcode> item)
			{
				SKMLSentineelControleType skmlSentineelControleType = item.getModelObject().getSentineelType();

				item.add(new Label("barcode.korteOmschrijving", skmlSentineelControleType.getKorteOmschrijving()));
				item.add(new TextField<>("barcode", new PropertyModel<String>(item.getModel(), "barcode")).setRequired(true));
				item.add(new TextField<>("qbase", new PropertyModel<Integer>(item.getModel(), "qbaseId")).setRequired(true));
			}
		};
		add(sentineelList);
	}
}
