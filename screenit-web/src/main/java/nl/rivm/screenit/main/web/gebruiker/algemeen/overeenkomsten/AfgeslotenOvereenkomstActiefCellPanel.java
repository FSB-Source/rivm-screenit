package nl.rivm.screenit.main.web.gebruiker.algemeen.overeenkomsten;

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

import nl.rivm.screenit.main.web.component.modal.IDialog;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class AfgeslotenOvereenkomstActiefCellPanel<T> extends GenericPanel<T>
{

	private static final long serialVersionUID = 1L;

	public AfgeslotenOvereenkomstActiefCellPanel(String id, IModel<T> rowModel, final IDialog dialog, final String confirmResourceKey)
	{
		super(id, rowModel);
		setOutputMarkupId(true);
		final WebMarkupContainer toggleActief = new WebMarkupContainer("toggleActief");

		toggleActief.add(new AjaxEventBehavior("click")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onEvent(AjaxRequestTarget target)
			{
				onToggleActief(target, getModelObject());
			}
		});

		toggleActief.setOutputMarkupId(true);
		if (!isActief(rowModel))
		{
			toggleActief.add(new AttributeAppender("class", Model.of(" niet-actief")));
		}
		else
		{
			toggleActief.add(new AttributeAppender("class", Model.of(" actief")));
		}
		add(toggleActief);
	}

	protected abstract boolean isActief(IModel<T> rowModel);

	protected abstract void onToggleActief(AjaxRequestTarget target, T actiefObject);
}
