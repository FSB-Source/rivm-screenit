package nl.rivm.screenit.main.web.component;

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

import java.util.Date;

import org.apache.wicket.extensions.markup.html.form.DateTextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.util.convert.IConverter;

public class ScreenitDateTextField extends DateTextField
{

	private static final long serialVersionUID = 1L;

	public ScreenitDateTextField(String id)
	{
		super(id);
	}

	public ScreenitDateTextField(String id, IModel<Date> model)
	{
		super(id, model);
	}

	public ScreenitDateTextField(String id, String datePattern)
	{
		super(id, datePattern);
	}

	public ScreenitDateTextField(String id, IModel<Date> model, String datePattern)
	{
		super(id, model, datePattern);
	}

	@Override
	public <C> IConverter<C> getConverter(Class<C> type)
	{
		if (Date.class.isAssignableFrom(type))
		{
			return (IConverter<C>) new MultiDateConverter();
		}
		else
		{
			return super.getConverter(type);
		}
	}

}
