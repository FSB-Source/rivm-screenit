package nl.rivm.screenit.main.web.component;

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

import nl.rivm.screenit.main.web.component.modal.ConfirmationBehavior;
import nl.rivm.screenit.main.web.component.modal.IDialog;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.StringResourceModel;

public abstract class ConfirmingIndicatingAjaxLink<T> extends IndicatingAjaxLink<T>
{

	private static final long serialVersionUID = 1L;

	private final IDialog popup;

	private final String resourceKey;

	private final boolean hasModel;

	public ConfirmingIndicatingAjaxLink(String wicketId, IModel<T> model, IDialog popup, String resourceKey)
	{
		super(wicketId, model);

		this.popup = popup;
		this.resourceKey = resourceKey;

		this.hasModel = model != null;
	}

	public ConfirmingIndicatingAjaxLink(String wicketId, IDialog popup, String resourceKey)
	{
		this(wicketId, null, popup, resourceKey);
	}

	@Override
	protected AjaxEventBehavior newAjaxEventBehavior(String event)
	{
		return new ConfirmationBehavior(popup)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected IModel<T> getModel()
			{
				if (hasModel)
				{
					return ConfirmingIndicatingAjaxLink.this.getModel();
				}
				else
				{
					return null;
				}
			}

			@Override
			protected void onEvent(AjaxRequestTarget target)
			{
				if (skipConfirmation())
				{
					onYesClick(target);
				}
				else if (continueConfirmation())
				{
					super.onEvent(target);
				}
			}

			@Override
			public void onCloseClick(AjaxRequestTarget target)
			{
				super.onCloseClick(target);
				ConfirmingIndicatingAjaxLink.this.onClose(target);
			}

			@Override
			public void onNoClick(AjaxRequestTarget target)
			{
				super.onNoClick(target);
				ConfirmingIndicatingAjaxLink.this.onNo(target);
			}

			@Override
			protected IModel<String> getContentStringModel()
			{
				return ConfirmingIndicatingAjaxLink.this.getContentStringModel();
			}

			@Override
			protected IModel<String> getHeaderStringModel()
			{
				return ConfirmingIndicatingAjaxLink.this.getHeaderStringModel();
			}

			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				super.updateAjaxAttributes(attributes);
				ConfirmingIndicatingAjaxLink.this.updateAjaxAttributes(attributes);
			}
		};
	}

	protected void onClose(AjaxRequestTarget target)
	{

	}

	protected void onNo(AjaxRequestTarget target)
	{

	}

	protected IModel<String> getContentStringModel()
	{
		IModel<T> model = null;
		if (hasModel)
		{
			model = getModel();
		}
		return new StringResourceModel(resourceKey + ".content", this, model).setDefaultValue("");
	}

	protected IModel<String> getHeaderStringModel()
	{
		IModel<T> model = null;
		if (hasModel)
		{
			model = getModel();
		}
		return new StringResourceModel(resourceKey + ".header", this, model);
	}

	protected boolean skipConfirmation()
	{
		return false;
	}

	protected boolean continueConfirmation()
	{
		return true;
	}
}
