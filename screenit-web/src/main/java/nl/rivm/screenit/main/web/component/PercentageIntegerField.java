
package nl.rivm.screenit.main.web.component;

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

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Locale;

import nl.rivm.screenit.Constants;

import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.util.convert.IConverter;
import org.apache.wicket.util.convert.converter.BigDecimalConverter;
import org.apache.wicket.util.convert.converter.IntegerConverter;
import org.apache.wicket.validation.IValidationError;
import org.apache.wicket.validation.ValidationError;
import org.apache.wicket.validation.validator.RangeValidator;

public class PercentageIntegerField extends TextField<Integer>
{

	private static final long serialVersionUID = 1L;

	public PercentageIntegerField(String id)
	{
		this(id, null);
	}

	public PercentageIntegerField(String id, int min)
	{
		this(id, null, min);
	}

	public PercentageIntegerField(String id, IModel<Integer> model)
	{
		this(id, model, 0, 10000);
	}

	public PercentageIntegerField(String id, IModel<Integer> model, int min)
	{
		this(id, model, min, 10000);
	}

	public PercentageIntegerField(String id, IModel<Integer> model, int min, int max)
	{
		super(id, model);
		setType(Double.class);
		add(RangeValidator.range(min, max));
	}

	@Override
	public void error(IValidationError error)
	{
		if (error instanceof ValidationError)
		{
			ValidationError valError = (ValidationError) error;
			Integer minimum = (Integer) valError.getVariables().get("minimum");
			Integer maximum = (Integer) valError.getVariables().get("maximum");

			if (minimum != null && maximum != null)
			{
				valError.getKeys().clear();
				valError.addKey("PercentageValidator.range");
				Double min = Double.valueOf(minimum) / 100.0;
				Double max = Double.valueOf(maximum) / 100.0;
				valError.getVariables().put("maximum", max.toString());
				valError.getVariables().put("minimum", min.toString());
			}
		}
		super.error(error);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <C> IConverter<C> getConverter(Class<C> type)
	{
		return (IConverter<C>) new PercentageIntegerConverter();
	}

	private class PercentageIntegerConverter extends IntegerConverter
	{

		private static final long serialVersionUID = 1L;

		private final BigDecimalConverter bigDecimalConverter;

		public PercentageIntegerConverter()
		{
			bigDecimalConverter = new BigDecimalConverter()
			{
				@Override
				protected NumberFormat newNumberFormat(Locale locale)
				{
					NumberFormat nf = NumberFormat.getNumberInstance(Constants.LOCALE_NL);
					DecimalFormat df = (DecimalFormat) nf;
					df.applyPattern("##0.00");
					return df;
				}
			};
		}

		@Override
		public Integer convertToObject(String value, Locale locale)
		{
			return bigDecimalConverter.convertToObject(value, locale).multiply(BigDecimal.valueOf(100)).intValue();
		}

		@Override
		public String convertToString(Integer value, Locale locale)
		{

			return bigDecimalConverter.convertToString(BigDecimal.valueOf(value).divide(BigDecimal.valueOf(100)), locale);
		}
	}
}
