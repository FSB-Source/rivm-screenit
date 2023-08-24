package nl.rivm.screenit.main.web.component.dropdown;

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
import java.util.List;

import nl.topicuszorg.wicket.input.RequiredSubmitComponent;

import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.IFormSubmittingComponent;
import org.apache.wicket.model.IModel;

public class RequiredScreenitDropdown<T> extends ScreenitDropdown<T> implements RequiredSubmitComponent
{

	private List<IFormSubmittingComponent> formSubmittingComponents;

	public RequiredScreenitDropdown(String id, IModel<? extends List<? extends T>> choices, IChoiceRenderer<? super T> renderer, IFormSubmittingComponent formSubmittingComponent)
	{
		super(id, choices, renderer);
		addFormSubmittingComponent(formSubmittingComponent);
	}

	@Override
	public boolean isRequired()
	{
		Form<?> form = findParent(Form.class);
		boolean required = false;
		if (formSubmittingComponents != null && form != null)
		{
			required = formSubmittingComponents.contains(findSubmittingButton(form.getRootForm()));
		}
		return required;
	}

	public void addFormSubmittingComponent(IFormSubmittingComponent formSubmittingComponent)
	{
		if (formSubmittingComponent == null)
		{
			throw new IllegalArgumentException("formSubmittingComponent == null");
		}
		else
		{
			if (this.formSubmittingComponents == null)
			{
				this.formSubmittingComponents = new ArrayList();
			}

			this.formSubmittingComponents.add(formSubmittingComponent);
		}
	}
}
