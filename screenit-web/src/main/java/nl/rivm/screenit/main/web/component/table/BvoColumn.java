
package nl.rivm.screenit.main.web.component.table;

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

import java.util.List;

import nl.rivm.screenit.model.IBevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class BvoColumn<T extends IBevolkingsonderzoek> extends AbstractColumn<T, String>
{

	private static final long serialVersionUID = 1L;

	public BvoColumn(IModel<String> displayModel)
	{
		super(displayModel);
	}

	@Override
	public void populateItem(Item<ICellPopulator<T>> cellItem, String componentId, IModel<T> rowModel)
	{
		T object = rowModel.getObject();
		List<Bevolkingsonderzoek> onderzoeken = object.getBevolkingsonderzoeken();
		String labelTekst = null;
		if (onderzoeken.isEmpty())
		{
			labelTekst = "";
		}
		else
		{
			labelTekst = Bevolkingsonderzoek.getAfkortingen(onderzoeken);
		}
		cellItem.add(new Label(componentId, new Model<String>(labelTekst)));

	}

}
