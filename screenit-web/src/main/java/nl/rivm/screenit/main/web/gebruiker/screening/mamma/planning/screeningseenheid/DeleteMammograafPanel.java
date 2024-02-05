package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.screeningseenheid;

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

import nl.rivm.screenit.model.mamma.MammaMammograaf;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class DeleteMammograafPanel extends GenericPanel<MammaMammograaf>
{
	DeleteMammograafPanel(final String id, IModel<MammaMammograaf> model)
	{
		super(id, model);
		add(new IndicatingAjaxLink<Void>("delete")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				delete(target, DeleteMammograafPanel.this.getModel());
			}
		});
	}

	abstract protected void delete(final AjaxRequestTarget target, final IModel<MammaMammograaf> model);
}
