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

import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.UploadDocument;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;

public abstract class UploadDocumentColumn<T extends IDocument, S> extends PropertyColumn<T, S> implements INotClickableColumn
{
	private static final long serialVersionUID = 1L;

	private WebMarkupContainer refreshContainer;

	private FileValidator fileValidator;

	public UploadDocumentColumn(IModel<String> displayModel, String propertyExpression, WebMarkupContainer refreshContainer)
	{
		this(displayModel, propertyExpression, refreshContainer, null);
	}

	public UploadDocumentColumn(IModel<String> displayModel, String propertyExpression, WebMarkupContainer refreshContainer, FileValidator validator)
	{
		super(displayModel, propertyExpression);
		this.refreshContainer = refreshContainer;
		this.fileValidator = validator;
	}

	@Override
	public void populateItem(Item<ICellPopulator<T>> item, String componentId, IModel<T> rowModel)
	{
		item.add(new UploadDocumentPanel<T>(componentId, rowModel, refreshContainer, fileValidator)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void saveUploadDocument(AjaxRequestTarget target, T ModelObject, UploadDocument document)
			{
				saveNewUploadDocument(target, ModelObject, document);
			}
		});
	}

	protected abstract void saveNewUploadDocument(AjaxRequestTarget target, T object, UploadDocument document);
}
