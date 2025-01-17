
package nl.rivm.screenit.main.web.gebruiker.screening.colon.gebieden;

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

import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.service.GemeenteService;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class GemeentePaspoortPanel extends GenericPanel<Gemeente>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private GemeenteService gemeenteService;

	public GemeentePaspoortPanel(String id, IModel<Gemeente> model)
	{
		super(id, model);
		add(new Label("naam"));
		add(new Label("code"));
		Gemeente gemeente = model.getObject();

		int aantal = gemeente.getUitnodigingsGebieden().size();
		add(new Label("aantal", Model.of(aantal)));
		String gesplitstOp = "";

		final Boolean gesplitsOpPostcode = gemeenteService.getGesplitsOpPostcode(gemeente);
		if (gesplitsOpPostcode != null)
		{
			if (gesplitsOpPostcode)
			{
				gesplitstOp = "Postcode";
			}
			else
			{
				gesplitstOp = "Woonplaats";
			}
		}
		add(new Label("gesplitstOp", gesplitstOp));
	}

}
