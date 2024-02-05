package nl.rivm.screenit.main.web.component.validator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.util.DateUtil;

import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;

public class MaximalePeriodeLengteValidator extends AbstractFormValidator
{

	private final FormComponent<Date> beginDatumInput;

	private final FormComponent<Date> eindDatumInput;

	private final int maxLength;

	public MaximalePeriodeLengteValidator(FormComponent<Date> beginDatum, FormComponent<Date> eindDatum, Integer maxLength)
	{
		this.beginDatumInput = beginDatum;
		this.eindDatumInput = eindDatum;
		this.maxLength = maxLength;
	}

	@Override
	@SuppressWarnings("unchecked")
	public FormComponent<Date>[] getDependentFormComponents()
	{
		return new FormComponent[] { beginDatumInput, eindDatumInput };
	}

	@Override
	public void validate(Form<?> form)
	{
		Date beginDatum = beginDatumInput.getConvertedInput();
		Date eindDatum = eindDatumInput.getConvertedInput();

		if (beginDatum != null && eindDatum != null)
		{
			var verschilInMaanden = DateUtil.getMonthsBetweenDates(beginDatum, eindDatum);

			if (maxLength < verschilInMaanden)
			{
				Map<String, Object> errorKey = new HashMap<>();
				errorKey.put("maximaleLengte", maxLength);
				error(eindDatumInput, errorKey);
			}
		}
	}
}
