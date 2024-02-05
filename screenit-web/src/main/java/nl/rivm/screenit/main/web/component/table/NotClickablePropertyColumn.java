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

import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.model.IModel;

public class NotClickablePropertyColumn<T, S> extends PropertyColumn<T, S> implements INotClickableColumn
{
	private static final long serialVersionUID = 1L;

	public NotClickablePropertyColumn(final IModel<String> displayModel, final S sortProperty,
		final String propertyExpression)
	{
		super(displayModel, sortProperty, propertyExpression);
	}

	public NotClickablePropertyColumn(final IModel<String> displayModel, final String propertyExpression)
	{
		this(displayModel, null, propertyExpression);
	}
}
