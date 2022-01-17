package nl.rivm.screenit.main.web.component.table;

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

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;

public abstract class AjaxLinkTableCellPanel<T> extends GenericPanel<T>
{
	private static final long serialVersionUID = 1L;

	private String resourceProperty;

	public AjaxLinkTableCellPanel(String id, IModel<T> rowModel)
	{
		this(id, rowModel, null);
	}

	public AjaxLinkTableCellPanel(String id, IModel<T> rowModel, String resourceProperty)
	{
		super(id, rowModel);
		this.resourceProperty = resourceProperty;
		setOutputMarkupId(true);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		final AjaxLink<T> ajaxLink = new AjaxLink<T>("link")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				AjaxLinkTableCellPanel.this.onClick(target, AjaxLinkTableCellPanel.this.getModel());
			}
		};
		ajaxLink.add(createLinkContent());
		add(ajaxLink);
	}

	protected Component createLinkContent()
	{
		if (resourceProperty != null)
		{
			return new Label("linkContent", getLocalizer().getString(resourceProperty, this, "Correctie"));
		}
		else
		{
			return new WebMarkupContainer("linkContent");
		}

	}

	protected abstract void onClick(AjaxRequestTarget target, IModel<T> iModel);
}
