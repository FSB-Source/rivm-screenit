package nl.rivm.screenit.main.web.component.table;

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

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigation;
import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigationIncrementLink;
import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigationLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractToolbar;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.AbstractLink;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.list.LoopItem;
import org.apache.wicket.markup.html.navigation.paging.IPageable;
import org.apache.wicket.model.IModel;

public class ScreenitPagingNavigation<T> extends AbstractToolbar
{

	private static final long serialVersionUID = 1L;

	private ScreenitDataTable<T, String> table;

	public ScreenitPagingNavigation(ScreenitDataTable<T, String> table, IModel<String> totaalLabel)
	{
		super(table);
		this.table = table;
		addNavigation(totaalLabel);
	}

	private void addNavigation(IModel<String> totaalLabel)
	{
		add(new Label("aantal", new IModel<String>()
		{
			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				return table.getItemCount() + "";
			}
		}));
		add(new Label("aantalLabel", totaalLabel));

		add(new AjaxPagingNavigationIncrementLink("vorige", table, -1)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				super.onClick(target);
				table.updateContainer(target);
			}

		});
		add(new AjaxPagingNavigationIncrementLink("volgende", table, 1)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				super.onClick(target);
				table.updateContainer(target);
			}

		});
		add(new AjaxPagingNavigation("navigation", table)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected Link<?> newPagingNavigationLink(String id, IPageable pageable, long pageIndex)
			{
				return new AjaxPagingNavigationLink(id, pageable, pageIndex)
				{

					private static final long serialVersionUID = 1L;

					@Override
					public void onClick(AjaxRequestTarget target)
					{
						super.onClick(target);
						table.updateContainer(target);
					}

				}.setAutoEnable(false);
			}

			@Override
			protected void populateItem(final LoopItem loopItem)
			{

				final long pageIndex = getStartIndex() + loopItem.getIndex();

				WebMarkupContainer container = new WebMarkupContainer("container");
				container.add(new AttributeAppender("class", new IModel<String>()
				{
					private static final long serialVersionUID = 1L;

					@Override
					public String getObject()
					{
						if (table.getCurrentPage() == pageIndex)
						{
							return "active";
						}

						return "";
					}
				}));
				final AbstractLink link = newPagingNavigationLink("pageLink", pageable, pageIndex);
				container.add(link);
				loopItem.add(container);

				String label = "";
				if (labelProvider != null)
				{
					label = labelProvider.getPageLabel(pageIndex);
				}
				else
				{
					label = String.valueOf(pageIndex + 1);
				}
				link.add(new Label("pageNumber", label));
			}
		});
	}
}
