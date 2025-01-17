package nl.rivm.screenit.main.web.gebruiker.screening.cervix.facturatie;

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

import nl.rivm.screenit.main.web.component.table.UploadDocumentDownloadColumn;

import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;

public class CervixBetalingSepaUploadDocumentColumn<T, S> extends UploadDocumentDownloadColumn<T, S>
{

	public CervixBetalingSepaUploadDocumentColumn(IModel displayModel, String propertyExpression)
	{
		super(displayModel, propertyExpression);
	}

	@Override
	public void populateItem(Item cellItem, String componentId, IModel rowModel)
	{
		cellItem.add(new CervixBetalingSepaUploadDocumentDownloadLinkPanel(componentId, (IModel) getDataModel(rowModel))
		{
			@Override
			protected void loggingBijOnClick()
			{
				if (CervixBetalingSepaUploadDocumentColumn.this.isEnabled(rowModel))
				{
					CervixBetalingSepaUploadDocumentColumn.this.loggingBijOnClick(rowModel);
				}
			}

			@Override
			public boolean isEnabled()
			{
				return CervixBetalingSepaUploadDocumentColumn.this.isEnabled(rowModel);
			}
		});
	}

	protected void loggingBijOnClick(IModel<T> rowModel)
	{
		return;
	}

	protected boolean isEnabled(IModel<T> rowModel)
	{
		return true;
	}
}
