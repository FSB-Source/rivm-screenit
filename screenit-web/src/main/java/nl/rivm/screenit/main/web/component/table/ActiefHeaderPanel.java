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

import nl.rivm.screenit.model.IActief;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class ActiefHeaderPanel<T extends IActief> extends Panel
{

	private static final long serialVersionUID = 1L;

	private final IModel<T> searchObjectModel;

	private final MarkupContainer refreshConainter;

	private IModel<String> displayModel;

	public ActiefHeaderPanel(String id, MarkupContainer refreshConainter, IModel<T> searchObjectModel)
	{
		this(id, refreshConainter, searchObjectModel, null);
	}

	public ActiefHeaderPanel(String componentId, MarkupContainer refreshConainter, IModel<T> searchObjectModel, IModel<String> displayModel)
	{
		super(componentId);
		this.refreshConainter = refreshConainter;
		this.searchObjectModel = searchObjectModel;
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
		if (getSearchObject().getActief() == null)
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
		if (Boolean.TRUE.equals(getSearchObject().getActief()))
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
		if (Boolean.FALSE.equals(getSearchObject().getActief()))
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

	void changeActief(AjaxRequestTarget target, Boolean actief, Component link)
	{
		target.add(refreshConainter);
		target.appendJavaScript("changeInActief('" + link.getMarkupId() + "');");
		T searchObject = ModelUtil.nullSafeGet(searchObjectModel);
		searchObject.setActief(actief);
		onChangeActiefFilter(target, actief);
	}

	protected void onChangeActiefFilter(AjaxRequestTarget target, Boolean actief)
	{

	}

	protected IActief getSearchObject()
	{
		return ModelUtil.nullSafeGet(searchObjectModel);
	}

	@Override
	public void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(searchObjectModel);
	}
}
