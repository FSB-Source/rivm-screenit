package nl.rivm.screenit.main.web.component.table;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.Serializable;

import nl.rivm.screenit.main.web.ScreenitApplication;

import org.apache.wicket.Component;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.util.lang.Classes;

public class EnumPropertyColumn<T, S extends Serializable, E extends Enum<E>> extends PropertyColumn<T, S>
{

	private static final long serialVersionUID = 1L;

	private final Component resourceSource;

	public EnumPropertyColumn(IModel<String> displayModel, S sortProperty, String propertyExpression)
	{
		this(displayModel, sortProperty, propertyExpression, null);
	}

	public EnumPropertyColumn(IModel<String> displayModel, String propertyExpression)
	{
		this(displayModel, propertyExpression, null);
	}

	public EnumPropertyColumn(IModel<String> displayModel, S sortProperty, String propertyExpression, Component resourceSource)
	{
		super(displayModel, sortProperty, propertyExpression);
		this.resourceSource = resourceSource;
	}

	public EnumPropertyColumn(IModel<String> displayModel, String propertyExpression, Component resourceSource)
	{
		super(displayModel, propertyExpression);
		this.resourceSource = resourceSource;
	}

	@Override
	public IModel<?> getDataModel(IModel<T> rowModel)
	{
		Enum<E> enumValue = getEnumValue(rowModel);
		IModel<?> dataModel = null;
		if (enumValue != null)
		{
			dataModel = new Model(getResourceString(resourceKey(enumValue)));
		}
		else
		{
			dataModel = super.getDataModel(rowModel);
		}

		return dataModel;
	}

	protected final String getResourceString(String resourceKey)
	{
		String propertyValue;
		if (resourceSource != null)
		{
			propertyValue = resourceSource.getString(resourceKey);
		}
		else
		{
			propertyValue = ScreenitApplication.get().getResourceSettings().getLocalizer().getString(resourceKey, null);
		}
		return propertyValue;
	}

	protected final Enum<E> getEnumValue(IModel<T> rowModel)
	{
		IModel<?> propertyModel = super.getDataModel(rowModel);
		Object propertyObject = propertyModel.getObject();
		if (propertyObject instanceof Enum<?>)
		{
			return (Enum<E>) propertyObject;
		}
		return null;
	}

	protected String resourceKey(Enum<?> value)
	{
		return Classes.simpleName(value.getDeclaringClass()) + '.' + value.name();
	}

}
