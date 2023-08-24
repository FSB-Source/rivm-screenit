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

import java.io.Serializable;

import nl.rivm.screenit.model.enums.BriefType;

import org.apache.wicket.Component;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.util.lang.Classes;

public class BriefTypePropertyColumn<T, S extends Serializable> extends EnumPropertyColumn<T, S, BriefType>
{

	private static final long serialVersionUID = 1L;

	public BriefTypePropertyColumn(IModel<String> displayModel, S sortProperty, String propertyExpression, Component resourceSource)
	{
		super(displayModel, sortProperty, propertyExpression, resourceSource);
	}

	public BriefTypePropertyColumn(IModel<String> displayModel, S sortProperty, String propertyExpression)
	{
		super(displayModel, sortProperty, propertyExpression);
	}

	public BriefTypePropertyColumn(IModel<String> displayModel, String propertyExpression, Component resourceSource)
	{
		super(displayModel, propertyExpression, resourceSource);
	}

	public BriefTypePropertyColumn(IModel<String> displayModel, String propertyExpression)
	{
		super(displayModel, propertyExpression);
	}

	@Override
	public IModel<?> getDataModel(IModel<T> rowModel)
	{
		IModel<?> dataModel = super.getDataModel(rowModel);
		Enum<BriefType> enumValue = getEnumValue(rowModel);

		if (enumValue != null && !BriefType.valueOf(enumValue.name()).isActief())
		{
			dataModel = new Model(dataModel.getObject() + " " + getResourceString(Classes.simpleName(BriefType.class) + ".niet.meer.in.gebruik"));
		}
		return dataModel;
	}
}
