package nl.rivm.screenit.main.web.component.modal;

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

import nl.rivm.screenit.main.web.component.AssertTagNameBehavior;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.Model;

public class BootstrapDialog extends Panel implements IDialog
{

	private static final long serialVersionUID = 1L;

	private final CloseBehavior closeBehavior;

	private boolean addMoveClass = false;

	private boolean open;

	private IDialogCloseCallback dialogCloseCallback;

	public BootstrapDialog(String id)
	{
		this(id, new WebMarkupContainer(CONTENT_ID));
	}

	public BootstrapDialog(String id, Component content)
	{
		super(id);
		setOutputMarkupId(true);
		addMoveClass = true;

		add(new AssertTagNameBehavior("div"));
		add(new AttributeAppender("class", Model.of("modal hide" + (fade() ? " fade" : "")), " "));
		add(new AttributeAppender("role", "dialog"));
		add(new AttributeAppender("data-backdrop", "static"));

		closeBehavior = new CloseBehavior();
		add(closeBehavior);

		setContent(content);
	}

	@Override
	public void setContent(Component content)
	{
		content.setOutputMarkupId(true);
		addOrReplace(content);
	}

	@Override
	public void open(AjaxRequestTarget target)
	{
		open = true;
		target.add(this);
		addMoveClass = true;
		if (dialogCloseCallback != null)
		{
			target.appendJavaScript("showDialog('" + getMarkupId(true) + "').on('hidden', function () {" + closeBehavior.getJs() + "});");
		}
		else
		{
			target.appendJavaScript("showDialog('" + getMarkupId(true) + "');");
		}
	}

	public void openWith(AjaxRequestTarget target, Component content)
	{
		setContent(content);
		open(target);
	}

	private class CloseBehavior extends AbstractDefaultAjaxBehavior
	{

		private static final long serialVersionUID = 1L;

		@Override
		protected void respond(AjaxRequestTarget target)
		{
			open = false;
			if (dialogCloseCallback != null)
			{
				dialogCloseCallback.onCloseClick(target);
			}
			BootstrapDialog.this.onClose(target);
		}

		public CharSequence getJs()
		{
			return getCallbackScript();
		}
	}

	@Override
	public void close(AjaxRequestTarget target)
	{
		addMoveClass = true;
		target.appendJavaScript("$('#" + getMarkupId(true) + "').modal('hide');");
	}

	public BootstrapDialog addOpenerAttributesTo(Component component)
	{
		component.add(new AttributeModifier("data-toggle", "modal"));
		component.add(new AttributeModifier("href", "#" + getMarkupId(true)));
		return this;
	}

	@Override
	protected void onConfigure()
	{
		super.onConfigure();
		if (addMoveClass)
		{
			addMoveClass = false;
			add(new AttributeAppender("class", " moveModalDialog"));
		}
	}

	@Override
	public void setCloseCallback(IDialogCloseCallback dialogCloseCallback)
	{
		this.dialogCloseCallback = dialogCloseCallback;
	}

	@Override
	public void onClose(AjaxRequestTarget target)
	{

	}

	public boolean isOpen()
	{
		return open;
	}

	public boolean fade()
	{
		return true;
	}
}
