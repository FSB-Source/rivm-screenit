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

import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;

abstract public class NotClickableAbstractColumn<T, S> extends AbstractColumn<T, S> implements INotClickableColumn
{
	private static final long serialVersionUID = 1L;

	public NotClickableAbstractColumn(IModel<String> displayModel, S sortProperty)
	{
		super(displayModel, sortProperty);
	}

	public NotClickableAbstractColumn(IModel<String> displayModel)
	{
		super(displayModel);
	}

	@Override
	abstract public void populateItem(Item<ICellPopulator<T>> item, String s, IModel<T> iModel);
}
