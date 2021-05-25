package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren;

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

import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.util.NaamUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class CervixHuisartsLocatiePanel extends GenericPanel<CervixHuisartsLocatie>
{
	public CervixHuisartsLocatiePanel(String id, IModel<CervixHuisartsLocatie> huisartsLocatieModel, boolean magAanpassen)
	{
		super(id, huisartsLocatieModel);
		CervixHuisartsLocatie huisartsLocatie = getModelObject();

		add(new Label("huisarts", huisartsLocatie != null ? NaamUtil.getNaamHuisarts(huisartsLocatie.getHuisarts()) : null));
		add(new Label("huisarts.agbcode"));
		add(new Label("naam"));

		add(new AjaxLink<Void>("wijzig")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				wijzigHuisartsLocatie(target);
			}
		}.setVisible(magAanpassen));
	}

	public abstract void wijzigHuisartsLocatie(AjaxRequestTarget target);
}
