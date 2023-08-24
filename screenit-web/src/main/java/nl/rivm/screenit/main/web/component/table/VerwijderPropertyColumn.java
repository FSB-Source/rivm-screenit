package nl.rivm.screenit.main.web.component.table;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class VerwijderPropertyColumn<T, S> extends AbstractColumn<T, String> implements INotClickableColumn
{

	private static final long serialVersionUID = 1L;

	private BootstrapDialog dialog;

	private String dialogMessage;

	private String vervangendeTekst;

	protected VerwijderPropertyColumn(IModel<String> displayModel, String vervangendeTekst, BootstrapDialog dialog, String dialogMessage)
	{
		super(displayModel, null);
		this.dialog = dialog;
		this.dialogMessage = dialogMessage;
		this.vervangendeTekst = vervangendeTekst;
	}

	protected VerwijderPropertyColumn(IModel<String> displayModel, String vervangendeTekst)
	{
		this(displayModel, vervangendeTekst, null, null);
	}

	@Override
	public void populateItem(Item<ICellPopulator<T>> cellItem, String componentId, IModel<T> rowModel)
	{

		if (isButtonVisible(rowModel))
		{
			cellItem.add(new AjaxImageCellPanel<T>(componentId, rowModel, "icon-trash")
			{
				private static final long serialVersionUID = 1L;

				@Override
				protected void onClick(AjaxRequestTarget target)
				{
					if (dialog != null)
					{
						dialog.openWith(target,
							new ConfirmPanel(IDialog.CONTENT_ID, Model.of(dialogMessage), null, new DefaultConfirmCallback()
							{
								private static final long serialVersionUID = 1L;

								@Override
								public void onYesClick(AjaxRequestTarget target)
								{
									onClickDeleteAction(target, rowModel);
								}
							}, dialog));
					}
					else
					{
						onClickDeleteAction(target, rowModel);
					}
				}
			});
		}
		else if (StringUtils.isNotEmpty(vervangendeTekst))
		{
			cellItem.add(new Label(componentId, Model.of(vervangendeTekst)));
		}
	}

	public abstract void onClickDeleteAction(AjaxRequestTarget target, IModel<T> rowModel);

	protected boolean isButtonVisible(IModel<T> rowModel)
	{
		return true;
	}

	public void setVervangendeTekstVoorKnop(String vervangendeTekst)
	{
		this.vervangendeTekst = vervangendeTekst;
	}

	public void setDialog(BootstrapDialog dialog, String dialogMessage)
	{
		this.dialog = dialog;
		this.dialogMessage = dialogMessage;
	}
}
