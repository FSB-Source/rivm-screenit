package nl.rivm.screenit.main.web.component;

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

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class ScreenitIndicatingAjaxLinkPanel<T> extends GenericPanel<T>
{
	public ScreenitIndicatingAjaxLinkPanel(String id, IModel<T> model, String linkLabel)
	{
		super(id, model);

		IndicatingAjaxLink<T> link = new IndicatingAjaxLink<T>("link", getModel())
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ScreenitIndicatingAjaxLinkPanel.this.onClick(target, getModel());
			}
		};
		link.add(new Label("linkLabel", Model.of(linkLabel)));
		link.setOutputMarkupId(true);
		add(link);
	}

	public abstract void onClick(AjaxRequestTarget target, IModel<T> model);
}
