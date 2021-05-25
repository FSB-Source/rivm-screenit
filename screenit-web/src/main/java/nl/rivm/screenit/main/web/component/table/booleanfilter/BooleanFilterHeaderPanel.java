package nl.rivm.screenit.main.web.component.table.booleanfilter;

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

import org.apache.wicket.Component;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class BooleanFilterHeaderPanel<T> extends Panel
{

	private static final long serialVersionUID = 1L;

	private final MarkupContainer refreshContainer;

	private IModel<String> displayModel;

	public BooleanFilterHeaderPanel(String id, MarkupContainer refreshContainer)
	{
		this(id, refreshContainer, null);
	}

	public BooleanFilterHeaderPanel(String id, MarkupContainer refreshContainer, IModel<String> displayModel)
	{
		super(id);
		this.refreshContainer = refreshContainer;
		setOutputMarkupId(true);
		this.displayModel = displayModel;
		createHeader();
		createLinks();
	}

	protected void createLinks()
	{

		AjaxLink<Void> all = new AjaxLink<Void>("all")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				changeActief(target, null, this);
			}

		};
		all.setOutputMarkupId(true);
		if (getProperty() == null)
		{
			all.add(new AttributeAppender("class", Model.of(" active")));
		}
		add(all);

		AjaxLink<Void> active = new AjaxLink<Void>("active")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				changeActief(target, Boolean.TRUE, this);
			}
		};
		active.setOutputMarkupId(true);
		if (Boolean.TRUE.equals(getProperty()))
		{
			active.add(new AttributeAppender("class", Model.of(" active")));
		}
		add(active);

		AjaxLink<Void> inactive = new AjaxLink<Void>("inactive")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				changeActief(target, Boolean.FALSE, this);
			}

		};
		inactive.setOutputMarkupId(true);
		if (Boolean.FALSE.equals(getProperty()))
		{
			inactive.add(new AttributeAppender("class", Model.of(" active")));
		}
		add(inactive);
	}

	protected void createHeader()
	{
		Label headerLabel;
		if (displayModel != null)
		{
			headerLabel = new Label("header", displayModel);

		}
		else
		{
			headerLabel = new Label("header", new Model<String>(""));

		}

		headerLabel.setOutputMarkupId(true);
		add(headerLabel);
	}

	void changeActief(AjaxRequestTarget target, Boolean value, Component link)
	{
		target.add(refreshContainer);
		target.appendJavaScript("changeInActief('" + link.getMarkupId() + "');");
		changeProperty(value);
		onChangeActiefFilter(target, value);
	}

	protected abstract void changeProperty(Boolean value);

	protected abstract Boolean getProperty();

	protected abstract void onChangeActiefFilter(AjaxRequestTarget target, Boolean value);
}
