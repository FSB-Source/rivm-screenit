
package nl.rivm.screenit.main.web.gebruiker.algemeen.overeenkomsten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.component.table.INotClickableColumn;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;

import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;

public class GeneratedDocumentDownloadColumn extends AbstractColumn<AbstractAfgeslotenOvereenkomst, String>implements INotClickableColumn
{

	private static final long serialVersionUID = 1L;

	public GeneratedDocumentDownloadColumn(IModel<String> displayModel, String propertyExpression)
	{
		super(displayModel, propertyExpression);
	}

	@Override
	public void populateItem(Item<ICellPopulator<AbstractAfgeslotenOvereenkomst>> cellItem, String componentId, IModel<AbstractAfgeslotenOvereenkomst> rowModel)
	{
		cellItem.add(new GeneratedDocumentDownloadLinkPanel(componentId, rowModel));
	}

}
