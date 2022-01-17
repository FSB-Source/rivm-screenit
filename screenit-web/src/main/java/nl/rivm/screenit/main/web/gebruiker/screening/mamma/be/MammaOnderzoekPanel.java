package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.util.KeyValue;

import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.RepeatingView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaOnderzoekPanel extends AbstractBEAccordionPanel<MammaOnderzoek>
{
	@SpringBean
	private MammaBaseOnderzoekService baseOnderzoekService;

	public MammaOnderzoekPanel(String id, IModel<MammaOnderzoek> item)
	{
		super(id, item, Model.of("Onderzoek"), 4);
		super.setIngeklapt(false);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		panelContainer.add(new AttributeAppender("style", Model.of("padding: 0;"), " "));
		List<KeyValue> rows = baseOnderzoekService.vorigeRondeTeksten(getModelObject(), false);
		RepeatingView tableView = new RepeatingView("rows");
		for (KeyValue row : rows)
		{
			final WebMarkupContainer webMarkupContainer = new WebMarkupContainer(tableView.newChildId());
			webMarkupContainer.add(new Label("key", row.getKey()));
			webMarkupContainer.add(new Label("value", row.getValue()));
			tableView.add(webMarkupContainer);
		}
		panelContainer.add(tableView);
	}
}
