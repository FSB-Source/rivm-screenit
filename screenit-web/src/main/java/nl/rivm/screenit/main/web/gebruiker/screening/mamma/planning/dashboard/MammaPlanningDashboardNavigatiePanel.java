package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.dashboard;

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

import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.MammaSECapaciteitEditPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route.MammaRoutePage;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public class MammaPlanningDashboardNavigatiePanel extends GenericPanel<MammaScreeningsEenheid>
{

	private static final long serialVersionUID = 1L;

	public MammaPlanningDashboardNavigatiePanel(String id, IModel<MammaScreeningsEenheid> model)
	{
		super(id, model);

		add(new IndicatingAjaxLink<Void>("capaciteitsKalender")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new MammaSECapaciteitEditPage(MammaPlanningDashboardNavigatiePanel.this.getModelObject(), null));
			}
		});
		add(new IndicatingAjaxLink<Void>("route")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new MammaRoutePage(MammaPlanningDashboardNavigatiePanel.this.getModelObject()));
			}
		});
	}
}
