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

import nl.rivm.screenit.model.IActief;

import org.apache.wicket.MarkupContainer;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class ActiefHeaderInFormPanel<T extends IActief> extends ActiefHeaderPanel<T>
{

	private static final long serialVersionUID = 1L;

	public ActiefHeaderInFormPanel(String id, MarkupContainer refreshConainter, IModel<T> searchObjectModel)
	{
		super(id, refreshConainter, searchObjectModel);
	}

	@Override
	protected void createLinks()
	{
		AjaxSubmitLink all = new AjaxSubmitLink("all")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
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

		AjaxSubmitLink active = new AjaxSubmitLink("active")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
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

		AjaxSubmitLink inactive = new AjaxSubmitLink("inactive")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
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

}
