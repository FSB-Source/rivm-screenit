package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.route;

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

import nl.rivm.screenit.main.dao.mamma.MammaRouteDao;
import nl.rivm.screenit.main.service.mamma.MammaRouteService;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaStandplaatsOpmerkingenPanel;
import nl.rivm.screenit.model.mamma.MammaStandplaats;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaRouteStandplaatsOpmerkingenPanel extends GenericPanel<MammaStandplaats>
{

	@SpringBean
	private MammaRouteDao routeDao;

	@SpringBean
	private MammaRouteService routeService;

	public MammaRouteStandplaatsOpmerkingenPanel(String id, IModel<MammaStandplaats> model)
	{
		super(id);

		add(new Label("naam", model.getObject().getNaam()));

		add(new MammaStandplaatsOpmerkingenPanel("opmerkingen", model));

		add(new AjaxLink<Void>("close")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				close(target);
			}
		});
	}

	protected abstract void close(AjaxRequestTarget target);
}
