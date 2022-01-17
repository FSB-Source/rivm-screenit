package nl.rivm.screenit.main.web.component.table;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.UploadDocument;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;

public class UploadDocumentDownloadColumn<T, S> extends PropertyColumn<T, S> implements INotClickableColumn
{
	public UploadDocumentDownloadColumn(IModel<String> displayModel, String propertyExpression)
	{
		super(displayModel, propertyExpression);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void populateItem(Item<ICellPopulator<T>> cellItem, String componentId, IModel<T> rowModel)
	{
		cellItem.add(new UploadDocumentDownloadLinkPanel(componentId, (IModel<UploadDocument>) getDataModel(rowModel), getFileNameToLog(rowModel))
		{
			@Override
			protected void onBeforeDownloadClick(AjaxRequestTarget target)
			{
				UploadDocumentDownloadColumn.this.onBeforeDownloadClick(target, rowModel);
			}
		});
	}

	protected IModel<String> getFileNameToLog(IModel<T> rowModel)
	{
		return null;
	}

	protected void onBeforeDownloadClick(AjaxRequestTarget target, IModel<T> rowModel)
	{
	}

}
