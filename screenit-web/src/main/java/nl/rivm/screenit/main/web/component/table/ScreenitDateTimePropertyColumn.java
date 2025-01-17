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

import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.search.column.ClickablePropertyColumn;

import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;

public class ScreenitDateTimePropertyColumn<T, S> extends ClickablePropertyColumn<T, S>
{
	public static SimpleDateFormat getDateTimeFormat()
	{
		return new SimpleDateFormat(Constants.DEFAULT_DATE_TIME_FORMAT);
	}

	private final SimpleDateFormat format;

	public ScreenitDateTimePropertyColumn(IModel<String> displayModel, String datumColumn)
	{
		super(displayModel, datumColumn);
		format = getDateTimeFormat();
	}

	public ScreenitDateTimePropertyColumn(IModel<String> displayModel, String datumColumn, S sortProperty)
	{
		super(displayModel, sortProperty, datumColumn);
		format = getDateTimeFormat();
	}

	public ScreenitDateTimePropertyColumn(IModel<String> displayModel, String datumColumn, SimpleDateFormat format)
	{
		super(displayModel, datumColumn);
		this.format = format;
	}

	public ScreenitDateTimePropertyColumn(IModel<String> displayModel, String datumColumn, S sortProperty,
		SimpleDateFormat format)
	{
		super(displayModel, sortProperty, datumColumn);
		this.format = format;
	}

	@Override
	public IModel<Object> getDataModel(IModel<T> embeddedModel)
	{
		Object object = embeddedModel.getObject();
		PropertyModel<T> model = new PropertyModel<>(object, getPropertyExpression());
		Date date = null;

		Object modelObject = model.getObject();

		if (modelObject instanceof Date)
		{
			date = (Date) modelObject;
		}
		else if (modelObject instanceof Long)
		{
			date = new Date((Long) modelObject);
		}
		else if (modelObject instanceof LocalDateTime)
		{
			date = DateUtil.toUtilDate((LocalDateTime) modelObject);
		}
		else if (modelObject instanceof LocalDate)
		{
			date = DateUtil.toUtilDate((LocalDate) modelObject);
		}

		if (date == null || format == null)
		{
			return new Model("");
		}
		else
		{
			return new Model(format.format(date));
		}
	}
}
