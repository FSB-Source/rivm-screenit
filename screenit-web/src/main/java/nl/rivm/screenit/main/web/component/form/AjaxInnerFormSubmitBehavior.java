
package nl.rivm.screenit.main.web.component.form;

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

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes.Method;
import org.apache.wicket.markup.html.form.Button;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.IFormSubmitter;
import org.apache.wicket.markup.html.form.IFormSubmittingComponent;

public abstract class AjaxInnerFormSubmitBehavior extends AjaxEventBehavior
{
	private static final long serialVersionUID = 1L;

	private Form<?> __form;

	private boolean defaultProcessing = true;

	public AjaxInnerFormSubmitBehavior(String event)
	{
		this(null, event);
	}

	public AjaxInnerFormSubmitBehavior(Form<?> form, String event)
	{
		super(event);
		__form = form;

		if (form != null)
		{
			form.setOutputMarkupId(true);
		}
	}

	public final Form<?> getForm()
	{
		if (__form == null)
		{
			__form = findForm();

			if (__form == null)
			{
				throw new IllegalStateException("form was not specified in the constructor and cannot " + "be found in the hierarchy of the component this behavior "
					+ "is attached to: Component=" + getComponent().toString(false));
			}
		}
		return __form;
	}

	private IFormSubmittingComponent getFormSubmittingComponent()
	{
		IFormSubmittingComponent submittingComponent = null;
		Component component = getComponent();
		if (component instanceof IFormSubmittingComponent)
		{
			submittingComponent = (IFormSubmittingComponent) component;
		}
		return submittingComponent;
	}

	protected Form<?> findForm()
	{

		Component component = getComponent();
		if (component instanceof Form<?>)
		{
			return (Form<?>) component;
		}
		else
		{
			return component.findParent(Form.class);
		}
	}

	@Override
	protected void updateAjaxAttributes(AjaxRequestAttributes attributes)
	{
		super.updateAjaxAttributes(attributes);

		Form<?> form = getForm();
		attributes.setFormId(form.getMarkupId());

		String formMethod = form.getMarkupAttributes().getString("method");
		if (formMethod == null || "POST".equalsIgnoreCase(formMethod))
		{
			attributes.setMethod(Method.POST);
		}

		if (form.getRootForm().isMultiPart())
		{
			attributes.setMultipart(true);
			attributes.setMethod(Method.POST);
		}

		IFormSubmittingComponent submittingComponent = getFormSubmittingComponent();
		if (submittingComponent != null)
		{
			String submittingComponentName = submittingComponent.getInputName();
			attributes.setSubmittingComponentName(submittingComponentName);
		}
	}

	@Override
	protected void onEvent(final AjaxRequestTarget target)
	{
		getForm().onFormSubmitted(new AjaxFormSubmitter(this, target));
	}

	public static class AjaxFormSubmitter implements IFormSubmitter
	{
		private final AjaxInnerFormSubmitBehavior submitBehavior;

		private final AjaxRequestTarget target;

		private AjaxFormSubmitter(AjaxInnerFormSubmitBehavior submitBehavior, AjaxRequestTarget target)
		{
			this.submitBehavior = submitBehavior;
			this.target = target;
		}

		@Override
		public Form<?> getForm()
		{
			return submitBehavior.getForm();
		}

		public IFormSubmittingComponent getFormSubmittingComponent()
		{
			return submitBehavior.getFormSubmittingComponent();
		}

		@Override
		public boolean getDefaultFormProcessing()
		{
			return submitBehavior.getDefaultProcessing();
		}

		@Override
		public void onError()
		{
			submitBehavior.onError(target);
		}

		@Override
		public void onSubmit()
		{
			submitBehavior.onSubmit(target);
		}

		@Override
		public void onAfterSubmit()
		{
			submitBehavior.onAfterSubmit(target);
		}
	}

	protected void onAfterSubmit(AjaxRequestTarget target)
	{
	}

	protected void onSubmit(AjaxRequestTarget target)
	{
	}

	protected void onError(AjaxRequestTarget target)
	{
	}

	public boolean getDefaultProcessing()
	{
		return defaultProcessing;
	}

	public void setDefaultProcessing(boolean defaultProcessing)
	{
		this.defaultProcessing = defaultProcessing;
	}
}
