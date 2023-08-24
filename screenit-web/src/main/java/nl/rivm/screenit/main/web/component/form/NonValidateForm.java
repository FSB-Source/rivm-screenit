package nl.rivm.screenit.main.web.component.form;

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

import org.apache.wicket.Page;
import org.apache.wicket.markup.html.form.AbstractSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.IFormSubmitter;
import org.apache.wicket.model.IModel;
import org.apache.wicket.request.mapper.parameter.PageParameters;
import org.apache.wicket.util.visit.IVisit;
import org.apache.wicket.util.visit.IVisitor;

public class NonValidateForm<T extends Object> extends Form<T>
{

	private static final long serialVersionUID = 1L;

	private boolean bypassValidation = false;

	public NonValidateForm(String id, IModel<T> model)
	{
		super(id, model);
	}

	@Override
	public void process(IFormSubmitter submittingComponent)
	{

		final Page page = getPage();

		if (!isEnabledInHierarchy() || !isVisibleInHierarchy())
		{

			return;
		}

		validate();

		if (hasError() && Boolean.FALSE.equals(bypassValidation))
		{

			markFormComponentsInvalid();

			callOnError(submittingComponent);
		}
		else
		{

			markFormComponentsValid();

			beforeUpdateFormComponentModels();

			updateFormComponentModels();

			onValidateModelObjects();
			if (hasError())
			{
				callOnError(submittingComponent);
				return;
			}

			delegateSubmit(submittingComponent);
		}

		final PageParameters parameters = page.getPageParameters();
		if (parameters != null)
		{
			visitFormComponents(new IVisitor<FormComponent<?>, Void>()
			{
				@Override
				public void component(final FormComponent<?> formComponent, final IVisit<Void> visit)
				{
					parameters.remove(formComponent.getInputName());
				}
			});

			if (submittingComponent instanceof AbstractSubmitLink)
			{
				AbstractSubmitLink submitLink = (AbstractSubmitLink) submittingComponent;
				parameters.remove(submitLink.getInputName());
			}
		}
	}

	public boolean isBypassValidation()
	{
		return bypassValidation;
	}

	public void setBypassValidation(boolean bypassValidation)
	{
		this.bypassValidation = bypassValidation;
	}

}
