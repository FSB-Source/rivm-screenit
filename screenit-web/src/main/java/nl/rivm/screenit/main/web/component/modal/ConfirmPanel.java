package nl.rivm.screenit.main.web.component.modal;

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

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;

public class ConfirmPanel extends Panel
{
	private static final long serialVersionUID = 1L;

	public ConfirmPanel(String id, IModel<String> header, IModel<String> content, final IConfirmCallback callback, final IDialog dialog)
	{
		super(id, header);

		add(new Label("header", header));
		if (content != null)
		{
			add(new Label("content", content).setVisible(StringUtils.isNotBlank(content.getObject())).setEscapeModelStrings(false));
		}
		else
		{
			add(new Label("content", "").setVisible(false));
		}
		add(createCustomComponent("customComponent"));

		add(new IndicatingAjaxLink<Void>("close")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.close(target);
				callback.onCloseClick(target);
			}
		});

		add(new IndicatingAjaxLink<Void>("ja")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.close(target);
				callback.onYesClick(target);
			}
		});
		add(new IndicatingAjaxLink<Void>("nee")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.close(target);
				callback.onNoClick(target);
			}
		});
	}

	protected Component createCustomComponent(String id)
	{
		return new WebMarkupContainer(id).setVisible(false);
	}
}
