
package nl.rivm.screenit.main.web.component;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.topicuszorg.wicket.form.ComponentValidatingForm;
import nl.topicuszorg.wicket.form.visitor.AdvancedInterfaceFormVisitor;
import nl.topicuszorg.wicket.form.visitor.border.RequiredBorder;
import nl.topicuszorg.wicket.input.AttributeRemover;
import nl.topicuszorg.wicket.input.RequiredSubmitComponent;

import org.apache.wicket.Application;
import org.apache.wicket.AttributeModifier;
import org.apache.wicket.Component;
import org.apache.wicket.RuntimeConfigurationType;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.behavior.Behavior;
import org.apache.wicket.feedback.FeedbackMessage;
import org.apache.wicket.feedback.IFeedbackMessageFilter;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.ListMultipleChoice;
import org.apache.wicket.markup.html.form.RadioGroup;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.form.ValidationErrorFeedback;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.util.visit.IVisit;
import org.apache.wicket.util.visit.IVisitor;
import org.apache.wicket.validation.ValidationError;

public class ScreenitForm<T> extends ComponentValidatingForm<T>
{

	private static final long serialVersionUID = 1L;

	private final List<FormComponent<?>> invalidFormComponents = new ArrayList<FormComponent<?>>();

	private class ValidatingFormVisitor extends AdvancedInterfaceFormVisitor
	{

		private static final long serialVersionUID = 1L;

		@Override
		protected Behavior getValidationMsgBehavior()
		{

			return null;
		}

		@Override
		protected Behavior getErrorHighlightBehavior()
		{
			return null;
		}

		@Override
		protected Behavior getComponentBorder(Component component)
		{
			return ScreenitForm.this.getComponentBorder(component);
		}

	}

	public ScreenitForm(String id)
	{
		super(id);
		setAdvancedInterfaceFormVisitor(new ValidatingFormVisitor());
	}

	public ScreenitForm(String id, IModel<T> model)
	{
		super(id, model);
		setAdvancedInterfaceFormVisitor(new ValidatingFormVisitor());
	}

	public ScreenitForm(String id, AdvancedInterfaceFormVisitor advancedInterfaceFormVisitor)
	{
		super(id, advancedInterfaceFormVisitor);
	}

	public ScreenitForm(String id, IModel<T> model, AdvancedInterfaceFormVisitor advancedInterfaceFormVisitor)
	{
		super(id, model, advancedInterfaceFormVisitor);
	}

	@Override
	protected void onError()
	{
		final AjaxRequestTarget target = RequestCycle.get().find(AjaxRequestTarget.class).orElse(null);
		if (target != null)
		{

			visitFormComponents(new IVisitor<FormComponent<?>, Void>()
			{

				@Override
				public void component(FormComponent<?> formComponent, IVisit<Void> formVisitor)
				{
					Component errorComponent = getErrorComponent(formComponent);
					IModel<String> errorClass = getErrorClass();
					if (!formComponent.isValid())
					{
						if (errorComponent.getOutputMarkupId())
						{
							errorComponent.add(new AttributeAppender("class", errorClass, " "));

							destroyWiqueryPlugin(target, formComponent);
							target.add(errorComponent);
							if (!invalidFormComponents.contains(formComponent))
							{
								invalidFormComponents.add(formComponent);
							}
						}
						else if (invalidFormComponents.contains(formComponent))
						{
							errorComponent.add(new AttributeRemover("class", errorClass, " "));

							destroyWiqueryPlugin(target, formComponent);
							target.add(errorComponent);
							invalidFormComponents.remove(formComponent);
						}

					}
					else if (invalidFormComponents.contains(formComponent))
					{
						errorComponent.add(new AttributeRemover("class", errorClass, " "));
						destroyWiqueryPlugin(target, formComponent);
						target.add(errorComponent);
						invalidFormComponents.remove(formComponent);
					}

					visitFormComponent(target, (FormComponent<T>) formComponent);
				}

			});

		}
	}

	@Override
	public void refreshFeedbackPanels(AjaxRequestTarget target)
	{

	}

	@Override
	protected void visitFormComponent(AjaxRequestTarget target, FormComponent<T> formComponent)
	{

		if (formComponent instanceof TextField<?>)
		{
			if (((TextField<?>) formComponent).isVisibleInHierarchy())
			{

				if (!formComponent.getBehaviors().isEmpty())
				{
					for (Behavior behavior : formComponent.getBehaviors())
					{
						if (behavior instanceof RequiredBorder)
						{
							formComponent.remove(behavior);
						}
					}
				}
				if (Date.class.equals(((TextField<?>) formComponent).getType()))
				{
					target.add(formComponent);
				}
			}
		}
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		if (!RuntimeConfigurationType.DEVELOPMENT.equals(Application.get().getConfigurationType()))
		{
			add(new AttributeModifier("autocomplete", Model.of("off")));
		}
	}

	public IModel<String> getErrorClass()
	{
		return Model.of("error");
	}

	@Override
	protected void onSubmit()
	{
		super.onSubmit();

		final AjaxRequestTarget target = RequestCycle.get().find(AjaxRequestTarget.class).orElse(null);
		if (target != null)
		{
			for (FormComponent<?> formComponent : invalidFormComponents)
			{
				destroyWiqueryPlugin(target, formComponent);

				getErrorComponent(formComponent).add(new AttributeRemover("class", getErrorClass(), " "));
				target.add(formComponent);
			}
			invalidFormComponents.clear();

			updateContainer(target);
		}
	}

	protected void updateContainer(AjaxRequestTarget target)
	{

	}

	@Override
	protected void onValidate()
	{
		super.onValidate();
		if (!checkRequired())
		{
			clearRequiredMessages(this);

			visitChildren(Component.class, new IVisitor<Component, Boolean>()
			{
				@Override
				public void component(final Component component, final IVisit<Boolean> visit)
				{
					clearRequiredMessages(component);
				}
			});

		}

	}

	protected boolean checkRequired()
	{
		return true;
	}

	private void clearRequiredMessages(Component comp)
	{
		comp.getFeedbackMessages().clear(new IFeedbackMessageFilter()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public boolean accept(FeedbackMessage message)
			{
				if (message.getLevel() == FeedbackMessage.ERROR)
				{
					FormComponent<?> comp = (FormComponent<?>) message.getReporter();
					if (message.getMessage() instanceof ValidationErrorFeedback)
					{
						ValidationErrorFeedback errorFeedback = (ValidationErrorFeedback) message.getMessage();
						if (errorFeedback.getError() instanceof ValidationError)
						{
							ValidationError validationError = (ValidationError) errorFeedback.getError();
							if (comp.isRequired() && validationError.getKeys().contains("Required"))
							{
								return true;
							}
						}
					}
				}
				return false;
			}
		});
	}

	protected Component getErrorComponent(FormComponent<?> formComponent)
	{
		Component component = formComponent;

		if (formComponent instanceof RadioGroup || formComponent instanceof DropDownChoice || formComponent instanceof TextArea || formComponent instanceof ListMultipleChoice)
		{
			component = formComponent.getParent();
		}

		return component;
	}

	protected Behavior getComponentBorder(Component component)
	{
		RequiredBorder border = new RequiredBorder();
		if (component instanceof RequiredSubmitComponent)
		{
			border.setUseRestriction(false);
		}
		return border;
	}

}
