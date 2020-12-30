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

import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.IConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.form.AjaxFormSubmitBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.StringResourceModel;

public abstract class ConfirmingIndicatingAjaxSubmitLink<T> extends IndicatingAjaxButton implements IConfirmCallback
{

	private static final long serialVersionUID = 1L;

	private final IDialog popup;

	private final String resourceKey;

	private final boolean hasModel;

	public ConfirmingIndicatingAjaxSubmitLink(String wicketId, Form<?> form, IDialog popup, String resourceKey)
	{
		super(wicketId, form);

		this.popup = popup;
		this.resourceKey = resourceKey;

		this.hasModel = form != null && form.getModel() != null;
	}

	public ConfirmingIndicatingAjaxSubmitLink(String wicketId, IDialog popup, String resourceKey)
	{
		this(wicketId, null, popup, resourceKey);
	}

	@Override
	protected AjaxFormSubmitBehavior newAjaxFormSubmitBehavior(String event)
	{
		return new AjaxFormSubmitBehavior(getForm(), event)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (skipConfirmation())
				{
					ConfirmingIndicatingAjaxSubmitLink.this.onYesClick(target);
				}
				else if (continueConfirmation())
				{
					target.appendJavaScript("$('.modal.fade.in').addClass('previousDialog').modal('hide');");
					popup.setContent(createPopup());
					popup.open(target);
				}
			}

			@Override
			protected void onAfterSubmit(AjaxRequestTarget target)
			{
				ConfirmingIndicatingAjaxSubmitLink.this.onAfterSubmit(target);
			}

			@Override
			protected void onError(AjaxRequestTarget target)
			{
				ConfirmingIndicatingAjaxSubmitLink.this.onError(target);
			}

			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				super.updateAjaxAttributes(attributes);
				ConfirmingIndicatingAjaxSubmitLink.this.updateAjaxAttributes(attributes);
			}

			@Override
			public boolean getDefaultProcessing()
			{
				return ConfirmingIndicatingAjaxSubmitLink.this.getDefaultFormProcessing();
			}

			protected Panel createPopup()
			{
				return new ConfirmPanel(IDialog.CONTENT_ID, 
					getHeaderStringModel(), 
					getContentStringModel(), 
					ConfirmingIndicatingAjaxSubmitLink.this, popup);
			}

		};
	}

	protected IModel<String> getContentStringModel()
	{
		IModel<T> model = null;
		if (hasModel)
		{
			model = (IModel<T>) getForm().getModel();
		}
		return new StringResourceModel(resourceKey + ".content", this, model).setDefaultValue("");
	}

	protected IModel<String> getHeaderStringModel()
	{
		IModel<T> model = null;
		if (hasModel)
		{
			model = (IModel<T>) getForm().getModel();
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

	@Override
	public void onYesClick(AjaxRequestTarget target)
	{
		onSubmit(target);
	}

	@Override
	public void onNoClick(AjaxRequestTarget target)
	{
		target.appendJavaScript("$('.previousDialog').not('#sessieVerlopenDialog').modal('show');");
	}

	@Override
	public void onCloseClick(AjaxRequestTarget target)
	{
		target.appendJavaScript("$('.previousDialog').not('#sessieVerlopenDialog').modal('show');");
	}

}
