package nl.rivm.screenit.main.web.client.dashboard.huisartswizard;

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

import nl.rivm.screenit.main.model.BaseHuisartsModel;
import nl.rivm.screenit.main.model.EnovationHuisartsModel;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.EnovationHuisarts;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class ClientHuisartsBevestigenWizardPanel extends GenericPanel<EnovationHuisarts>
{
	private static final long serialVersionUID = 1L;

	public ClientHuisartsBevestigenWizardPanel(String id, IModel<EnovationHuisarts> geselecteerdHuisartsModel)
	{
		super(id, geselecteerdHuisartsModel);

		add(getHuisartsInfoComponent("huisartsInfo"));

		add(new IndicatingAjaxLink<Void>("terug")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				onZoekAndereHuisarts(target);
			}
		});
		add(new IndicatingAjaxLink<Void>("annuleren")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				onStop(target);
			}
		});
		add(new IndicatingAjaxLink<Void>("opslaan")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ScreenitSession.get().success(getString("message.gegevensopgeslagen"));
				onBevestigd(target, ClientHuisartsBevestigenWizardPanel.this.getModelObject());
			}
		});
	}

	protected WebMarkupContainer getHuisartsInfoComponent(String id)
	{
		return new ClientHuisartsInfoPanel(id, getHuisartsModel());
	}

	protected abstract void onBevestigd(AjaxRequestTarget target, EnovationHuisarts huisarts);

	protected abstract void onStop(AjaxRequestTarget target);

	protected abstract void onZoekAndereHuisarts(AjaxRequestTarget target);

	protected BaseHuisartsModel<?> getHuisartsModel()
	{
		return new EnovationHuisartsModel(getModelObject());
	}
}
