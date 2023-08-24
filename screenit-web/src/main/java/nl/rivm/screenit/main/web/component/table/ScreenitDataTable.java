
package nl.rivm.screenit.main.web.component.table;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.topicuszorg.wicket.search.AjaxLinkItem;
import nl.topicuszorg.wicket.search.IndicatingAjaxLinkDataTable;
import nl.topicuszorg.wicket.search.TextFilteredPropertyColumnNoWrap;

import org.apache.wicket.Component;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigation;
import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigationIncrementLink;
import org.apache.wicket.ajax.markup.html.navigation.paging.AjaxPagingNavigationLink;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.ISortState;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.ISortableDataProvider;
import org.apache.wicket.extensions.markup.html.repeater.util.SingleSortState;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.JavaScriptHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.link.AbstractLink;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.list.LoopItem;
import org.apache.wicket.markup.html.navigation.paging.IPageable;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.AbstractRepeater;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.markup.repeater.data.IDataProvider;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class ScreenitDataTable<T, S> extends IndicatingAjaxLinkDataTable<T, S>
{
	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(JavaScriptHeaderItem.forUrl("assets/js/mouse/mouseOverOut.js"));
	}

	private static final long serialVersionUID = 1L;

	public ScreenitDataTable(String id, List<IColumn<T, S>> columns, SortableDataProvider<T, S> dataProvider, IModel<String> totaalLabel)
	{
		this(id, columns, dataProvider, Constants.AANTAL_RIJEN, totaalLabel);
	}

	public ScreenitDataTable(String id, List<IColumn<T, S>> columns, SortableDataProvider<T, S> dataProvider, int aantal, IModel<String> totaalLabel)
	{
		super(id, columns, dataProvider, aantal);

		addNavigation(totaalLabel, true);
	}

	public ScreenitDataTable(String id, List<IColumn<T, S>> columns, SortableDataProvider<T, S> dataProvider, int aantal, IModel<String> totaalLabel,
		boolean pagingVisible)
	{
		super(id, columns, dataProvider, aantal);

		addNavigation(totaalLabel, pagingVisible);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		IDataProvider<T> dataProvider = getDataProvider();
		if (dataProvider instanceof SortableDataProvider)
		{
			ISortState<?> sortState = ScreenitSession.get().getCurrentSort(getClassRelativePath());
			if (sortState instanceof SingleSortState && getValuesFromSession())
			{
				((SortableDataProvider<T, S>) dataProvider).setSort(((SingleSortState<S>) sortState).getSort());
			}
			Long pageNumber = ScreenitSession.get().getSavedPageNumber(getClassRelativePath());
			if (pageNumber != null && (pageNumber = berekenDeJuisteNummer(pageNumber)) > 0)
			{
				setCurrentPage(pageNumber);
			}
		}
	}

	private void addNavigation(IModel<String> totaalLabel, boolean pagingVisible)
	{
		WebMarkupContainer totaalContainer = new WebMarkupContainer("totaal");
		totaalContainer.add(new Label("aantal", new IModel<String>()
		{
			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				return getItemCount() + "";
			}
		}));
		totaalContainer.add(new Label("aantalLabel", totaalLabel));
		totaalContainer.add(getCustomPanel("customPanel"));
		add(totaalContainer);
		totaalContainer.setVisible(totaalLabel != null);

		add(new AjaxPagingNavigationIncrementLink("vorige", this, -1)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				super.onClick(target);
				updateContainer(target);
				if (getValuesFromSession())
				{
					ScreenitSession.get().setSavedPageNumber(ScreenitDataTable.this.getClassRelativePath(), getPageNumber());
				}
			}

		}
			.setVisible(pagingVisible));

		add(new AjaxPagingNavigationIncrementLink("volgende", this, 1)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				super.onClick(target);
				updateContainer(target);
				if (getValuesFromSession())
				{
					ScreenitSession.get().setSavedPageNumber(ScreenitDataTable.this.getClassRelativePath(), getPageNumber());
				}
			}

		}
			.setVisible(pagingVisible));
		add(new AjaxPagingNavigation("navigation", this)
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
						updateContainer(target);
						if (getValuesFromSession())
						{
							ScreenitSession.get().setSavedPageNumber(ScreenitDataTable.this.getClassRelativePath(), getPageNumber());
						}
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
						if (getCurrentPage() == pageIndex)
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
		}.setVisible(pagingVisible));
	}

	public Panel getCustomPanel(String id)
	{
		return new EmptyPanel(id);
	}

	@Override
	public void onClick(AjaxRequestTarget target, IModel<T> model)
	{

	}

	@Override
	protected Item<T> newRowItem(String id, int index, IModel<T> model)
	{
		super.newRowItem(id, index, model);
		return new Item<T>(id, index, model)
		{
			@Override
			protected void onComponentTag(ComponentTag tag)
			{
				super.onComponentTag(tag);
				tag.put("class", getCssClass(getIndex(), getModel()));
			}
		};
	}

	private Long berekenDeJuisteNummer(Long paginaNumber)
	{

		Long pageCount;
		if ((pageCount = getPageCount()) < paginaNumber)
		{
			return pageCount;
		}
		return paginaNumber;
	}

	@Override
	protected Item<IColumn<T, S>> newCellItem(String id, int index, IModel<IColumn<T, S>> model)
	{
		Item<IColumn<T, S>> returnItem = null;

		final IColumn<T, S> column = model.getObject();

		if (isRowClickable(getRowModel()) && !(column instanceof INotClickableColumn))
		{

			returnItem = new AjaxLinkItem<T, S>(id, index, model, getRowModel())
			{
				private static final long serialVersionUID = 1L;

				@Override
				public void onClickItem(AjaxRequestTarget target, IModel<T> theModel)
				{
					ScreenitDataTable.this.onClick(target, theModel);
				}

				@Override
				protected void onComponentTag(ComponentTag tag)
				{
					super.onComponentTag(tag);
					tag.remove("onmouseover");
					tag.remove("onmouseout");
					tag.remove("style");
					tag.append("class", "js-on-mouse-over js-on-mouse-out move-handle ", " ");
				}
			};
		}
		else
		{
			returnItem = new Item<IColumn<T, S>>(id, index, model);
		}

		if (column instanceof TextFilteredPropertyColumnNoWrap<?, ?, ?>)
		{
			returnItem.add(new AttributeAppender("nowrap", Model.of(""), ""));
		}

		return returnItem;
	}

	@Override
	protected boolean addPager()
	{
		return false;
	}

	protected void updateContainer(AjaxRequestTarget target)
	{

		Component container = this;

		while (container instanceof AbstractRepeater)
		{
			container = container.getParent();
		}
		target.add(container);

		if (!((MarkupContainer) container).contains(this, true))
		{
			target.add(this);
		}
	}

	@Override
	protected void onSortChanged()
	{
		super.onSortChanged();
		IDataProvider<T> dataProvider = getDataProvider();
		if (dataProvider instanceof ISortableDataProvider)
		{
			ScreenitSession.get().setCurrentSort(getClassRelativePath(), ((ISortableDataProvider<T, S>) dataProvider).getSortState());
		}
	}

	protected boolean getValuesFromSession()
	{
		return true;
	}
}
