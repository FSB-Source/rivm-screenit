package nl.rivm.screenit.main.web.component.table;

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

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class AjaxImageCellPanel<T> extends GenericPanel<T>
{

	private static final long serialVersionUID = 1L;

	public AjaxImageCellPanel(String id, IModel<T> rowModel, final String imageClass)
	{
		super(id, rowModel);
		setOutputMarkupId(true);
		final AjaxLink<T> ajaxLink = new AjaxLink<T>("imageLink")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				AjaxImageCellPanel.this.onClick(target);
			}
		};
		ajaxLink.add(new WebMarkupContainer("image").add(new AttributeModifier("class", imageClass)));
		add(ajaxLink);
	}

	protected abstract void onClick(AjaxRequestTarget target);
}
