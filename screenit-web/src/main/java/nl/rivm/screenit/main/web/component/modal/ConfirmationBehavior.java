package nl.rivm.screenit.main.web.component.modal;

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

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxCallListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;

public abstract class ConfirmationBehavior extends AjaxEventBehavior implements IConfirmCallback
{
	private static final long serialVersionUID = 1L;

	private final IDialog popup;

	public ConfirmationBehavior(IDialog popup)
	{
		super("click");
		this.popup = popup;

	}

	@Override
	protected void onBind()
	{

		if (!(getComponent() instanceof Link<?> || getComponent() instanceof AjaxLink<?>))
		{
			throw new IllegalArgumentException("ConfirmationBehavior kan alleen op Link en AjaxLink elementen toegevoegd worden.");
		}
	}

	@Override
	protected void onEvent(AjaxRequestTarget target)
	{
		target.appendJavaScript("$('.modal.fade.in').not('#sessieVerlopenDialog').addClass('previousDialog').modal('hide');");
		popup.setContent(createPopup());
		popup.open(target);
	}

	protected Panel createPopup()
	{
		return new ConfirmPanel(IDialog.CONTENT_ID, 
			getHeaderStringModel(), 
			getContentStringModel(), 
			this, popup)
		{

			@Override
			protected void createCustomComponent(String id, Form<?> form)
			{
				ConfirmationBehavior.this.createCustomComponent(id, form);
			}
		};
	}

	protected abstract IModel<String> getContentStringModel();

	protected abstract IModel<String> getHeaderStringModel();

	protected void createCustomComponent(String id, Form<?> form)
	{
		form.add(new WebMarkupContainer(id).setVisible(false));
	}

	@Override
	public void onYesClick(AjaxRequestTarget target)
	{
		if (getComponent() instanceof IndicatingAjaxLink<?>)
		{
			((IndicatingAjaxLink<?>) getComponent()).onClick(target);
		}
		else if (getComponent() instanceof Link<?>)
		{
			((Link<?>) getComponent()).onClick();
		}
	}

	@Override
	public void onNoClick(AjaxRequestTarget target)
	{
		showPrevious(target);
	}

	protected void showPrevious(AjaxRequestTarget target)
	{
		target.appendJavaScript("$('.previousDialog').not('#sessieVerlopenDialog').modal('show');");
	}

	@Override
	public void onCloseClick(AjaxRequestTarget target)
	{
		showPrevious(target);
	}

	public IDialog getPopup()
	{
		return popup;
	}

	@Override
	protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
	{
		super.updateAjaxAttributes(attributes);
		attributes.getAjaxCallListeners().add(new AjaxCallListener().onPrecondition("return true;"));
	}

	protected IModel<?> getModel()
	{
		return null;
	}
}
