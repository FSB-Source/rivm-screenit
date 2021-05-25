package nl.rivm.screenit.main.web.component.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;

public class BigDecimalPuntFormValidator extends AbstractFormValidator
{

	private static final long serialVersionUID = 1L;

	private final List<FormComponent<BigDecimal>> components;

	public BigDecimalPuntFormValidator(FormComponent<BigDecimal>... components)
	{
		this(Arrays.asList(components));
	}

	public BigDecimalPuntFormValidator(List<FormComponent<BigDecimal>> components)
	{
		components.forEach(component -> {
			if (component == null)
			{
				throw new IllegalArgumentException("Argument formcomponent is null");
			}
		});
		this.components = components;
	}

	@Override
	public FormComponent<?>[] getDependentFormComponents()
	{
		return components.toArray(new FormComponent[components.size()]);
	}

	@Override
	public void validate(Form<?> form)
	{
		components.forEach(component -> {
			if (StringUtils.contains(component.getRawInput(), '.'))
			{
				error(component);
			}
		});
	}

}
