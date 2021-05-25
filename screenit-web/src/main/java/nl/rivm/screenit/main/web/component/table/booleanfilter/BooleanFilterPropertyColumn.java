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

import java.lang.reflect.InvocationTargetException;

import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.wicket.Component;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class BooleanFilterPropertyColumn<T, S> extends PropertyColumn<T, String>
{

	private static final long serialVersionUID = 1L;

	private String searchProperty;

	private IModel<S> searchObjectModel;

	private MarkupContainer refreshConainter;

	private final boolean verwijderen;

	private final IDialog dialog;

	private final String confirmResourceKey;

	private IModel<String> displayModel;

	public BooleanFilterPropertyColumn(Model<String> displayModel, String searchProperty, IModel<S> searchObjectModel, MarkupContainer refreshContainer)
	{
		this(displayModel, searchProperty, searchObjectModel, refreshContainer, null);
	}

	public BooleanFilterPropertyColumn(Model<String> displayModel, String searchProperty, IModel<S> searchObjectModel, MarkupContainer refreshContainer, String propertyExpression)
	{
		this(displayModel, searchProperty, searchObjectModel, refreshContainer, propertyExpression, false, null, null);
	}

	public BooleanFilterPropertyColumn(IModel<String> displayModel, String searchProperty, IModel<S> searchObjectModel, MarkupContainer refreshConainter, String propertyExpression,
		boolean verwijderen, IDialog dialog, String confirmResourceKey)
	{
		super(displayModel, propertyExpression);
		this.displayModel = displayModel;
		this.searchProperty = searchProperty;
		this.searchObjectModel = searchObjectModel;
		this.refreshConainter = refreshConainter;
		this.verwijderen = verwijderen;
		this.dialog = dialog;
		this.confirmResourceKey = confirmResourceKey;
		if (verwijderen && (dialog == null || confirmResourceKey == null))
		{
			throw new IllegalArgumentException("als inzien op false dan moet dialog en confirmResourceKey niet null zijn.");
		}

	}

	@Override
	public Component getHeader(String componentId)
	{
		return new BooleanFilterHeaderPanel<T>(componentId, refreshConainter, displayModel)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void changeProperty(Boolean value)
			{
				BooleanFilterPropertyColumn.this.setProperty(searchObjectModel, searchProperty, value);
			}

			@Override
			protected Boolean getProperty()
			{
				return BooleanFilterPropertyColumn.this.getProperty(searchObjectModel, searchProperty);
			}

			@Override
			protected void onChangeActiefFilter(AjaxRequestTarget target, Boolean value)
			{
				BooleanFilterPropertyColumn.this.onChangeActiefFilter(target, value);
			}

		};
	}

	protected void onChangeActiefFilter(AjaxRequestTarget target, Boolean value)
	{

	}

	protected void onToggleActief(AjaxRequestTarget target, T actiefObject)
	{

	}

	@Override
	public void populateItem(Item<ICellPopulator<T>> item, String componentId, IModel<T> rowModel)
	{
		item.add(new BooleanFilterCellPanel<T>(componentId, rowModel, getPropertyExpression(), verwijderen, dialog, confirmResourceKey)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected boolean isActief(IModel<T> rowModel)
			{
				return BooleanFilterPropertyColumn.this.isTrue(rowModel);
			}
		});
	}

	protected boolean isTrue(IModel<T> rowModel)
	{

		Boolean value = null;
		try
		{
			value = (Boolean) PropertyUtils.getProperty(rowModel.getObject(), getPropertyExpression());
		}
		catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
		{

		}
		return !Boolean.FALSE.equals(value);
	}

	private Boolean getProperty(IModel<S> model, String property)
	{
		try
		{
			return (Boolean) PropertyUtils.getProperty(model.getObject(), property);
		}
		catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
		{

		}
		return null;
	}

	private void setProperty(IModel<S> model, String property, Boolean value)
	{
		try
		{
			PropertyUtils.setProperty(model.getObject(), property, value);
		}
		catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
		{

		}
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(searchObjectModel);
	}

}
