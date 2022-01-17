package nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren;

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

import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.form.AjaxFormSubmitBehavior;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.html.form.Form;

public abstract class FormulierIndicatingAjaxSubmitLink extends ScreenitIndicatingAjaxSubmitLink
{

	private static final long serialVersionUID = 1L;

	public FormulierIndicatingAjaxSubmitLink(String id, Form<?> form)
	{
		super(id, form);
	}

	@Override
	protected AjaxFormSubmitBehavior newAjaxFormSubmitBehavior(String event)
	{
		return new AjaxFormSubmitBehavior(getForm(), event)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onError(AjaxRequestTarget target)
			{
				FormulierIndicatingAjaxSubmitLink.this.onError(target);
			}

			@Override
			protected Form<?> findForm()
			{
				return FormulierIndicatingAjaxSubmitLink.this.getForm();
			}

			@Override
			protected void onComponentTag(ComponentTag tag)
			{

				if (isEnabledInHierarchy())
				{
					super.onComponentTag(tag);
				}
			}

			@Override
			public boolean getDefaultProcessing()
			{
				return FormulierIndicatingAjaxSubmitLink.this.getDefaultFormProcessing();
			}

			@Override
			protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
			{
				super.updateAjaxAttributes(attributes);
				FormulierIndicatingAjaxSubmitLink.this.updateAjaxAttributes(attributes);
			}

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				FormulierIndicatingAjaxSubmitLink.this.onSubmit(target);
			}

			@Override
			protected void onAfterSubmit(AjaxRequestTarget target)
			{
				FormulierIndicatingAjaxSubmitLink.this.onAfterSubmit(target);
			}

			@Override
			protected void onEvent(AjaxRequestTarget target)
			{
				onBeforeHandleEvent();
				super.onEvent(target);
			}

		};

	}

	protected void onBeforeHandleEvent()
	{

	}

}
