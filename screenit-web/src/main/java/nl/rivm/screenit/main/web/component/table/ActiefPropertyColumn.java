package nl.rivm.screenit.main.web.component.table;

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

import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.model.IActief;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class ActiefPropertyColumn<T extends IActief, S extends IActief> extends PropertyColumn<T, String>implements INotClickableColumn
{

	private static final long serialVersionUID = 1L;

	private MarkupContainer refreshConainter;

	private IModel<S> searchObjectModel;

	private final boolean verwijderen;

	private final IDialog dialog;

	private final String confirmResourceKey;

	private IModel<String> displayModel;

	public ActiefPropertyColumn(IModel<String> displayModel, String propertyExpression, MarkupContainer refreshConainter, S searchObject, boolean verwijderen, IDialog dialog,
		String confirmResourceKey)
	{
		this(displayModel, propertyExpression, refreshConainter, new Model<S>(searchObject), verwijderen, dialog, confirmResourceKey);
	}

	public ActiefPropertyColumn(IModel<String> displayModel, String propertyExpression, MarkupContainer refreshConainter, IModel<S> searchObjectModel)
	{
		this(displayModel, propertyExpression, refreshConainter, searchObjectModel, false, null, null);
		this.displayModel = displayModel;
	}

	public ActiefPropertyColumn(IModel<String> displayModel, String propertyExpression, MarkupContainer refreshConainter, S searchObject)
	{
		this(displayModel, propertyExpression, refreshConainter, new Model<S>(searchObject), false, null, null);
	}

	public ActiefPropertyColumn(IModel<String> displayModel, String propertyExpression, MarkupContainer refreshConainter, IModel<S> searchObjectModel, boolean verwijderen,
		IDialog dialog, String confirmResourceKey)
	{
		super(displayModel, propertyExpression);
		this.refreshConainter = refreshConainter;
		this.searchObjectModel = searchObjectModel;
		this.verwijderen = verwijderen;
		this.dialog = dialog;
		this.confirmResourceKey = confirmResourceKey;
		if (verwijderen && (dialog == null || confirmResourceKey == null))
		{
			throw new IllegalArgumentException("als inzien op false dan moet dialog en confirmResourceKey niet null zijn.");
		}
	}

	@Override
	public Component getHeader(String componentId)
	{
		return new ActiefHeaderPanel<S>(componentId, refreshConainter, searchObjectModel, displayModel)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onChangeActiefFilter(AjaxRequestTarget target, Boolean actief)
			{
				ActiefPropertyColumn.this.onChangeActiefFilter(target, actief);
			}

		};
	}

	protected void onChangeActiefFilter(AjaxRequestTarget target, Boolean actief)
	{

	}

	@Override
	public void populateItem(Item<ICellPopulator<T>> item, String componentId, IModel<T> rowModel)
	{

		item.add(new ActiefCellPanel<T>(componentId, rowModel, verwijderen, dialog, confirmResourceKey)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected boolean mayToggle(T actiefObject)
			{
				return ActiefPropertyColumn.this.mayToggle(actiefObject);
			}

			@Override
			protected boolean isActief(IModel<T> rowModel)
			{
				return ActiefPropertyColumn.this.isActief(rowModel);
			}

			@Override
			protected void onAfterToggleActief(AjaxRequestTarget target, T actiefObject)
			{
				super.onAfterToggleActief(target, actiefObject);
				ActiefPropertyColumn.this.onAfterToggleActief(target, actiefObject);
			}

		});
	}

	protected boolean mayToggle(T actiefObject)
	{
		return true;
	}

	protected void onAfterToggleActief(AjaxRequestTarget target, T actiefObject)
	{

	}

	protected boolean isActief(IModel<T> rowModel)
	{
		return !Boolean.FALSE.equals(rowModel.getObject().getActief());
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(searchObjectModel);
	}

}
