package nl.rivm.screenit.main.web.component;

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

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Locale;

import nl.rivm.screenit.Constants;

import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.util.convert.IConverter;
import org.apache.wicket.util.convert.converter.BigDecimalConverter;
import org.apache.wicket.validation.validator.RangeValidator;

public class PercentageBigDecimalField extends TextField<BigDecimal>
{

	private static final long serialVersionUID = 1L;

	public PercentageBigDecimalField(String id)
	{
		this(id, null);
	}

	public PercentageBigDecimalField(String id, IModel<BigDecimal> model)
	{
		this(id, model, BigDecimal.valueOf(0), BigDecimal.valueOf(1));
	}

	public PercentageBigDecimalField(String id, IModel<BigDecimal> model, BigDecimal min)
	{
		this(id, model, min, BigDecimal.valueOf(1));
	}

	public PercentageBigDecimalField(String id, IModel<BigDecimal> model, BigDecimal min, BigDecimal max)
	{
		super(id, model);
		setType(Double.class);
		add(RangeValidator.range(min, max));
	}

	@SuppressWarnings("unchecked")
	@Override
	public <C> IConverter<C> getConverter(Class<C> type)
	{
		return (IConverter<C>) new PercentageConverterDouble();
	}

	private class PercentageConverterDouble extends BigDecimalConverter
	{

		private static final long serialVersionUID = 1L;

		private final BigDecimalConverter bigDecimalConverter;

		public PercentageConverterDouble()
		{
			bigDecimalConverter = new BigDecimalConverter()
			{
				@Override
				protected NumberFormat newNumberFormat(Locale locale)
				{
					NumberFormat nf = NumberFormat.getNumberInstance(Constants.LOCALE_NL);
					DecimalFormat df = (DecimalFormat) nf;
					df.applyPattern("##0");
					return df;
				}
			};
		}

		@Override
		public BigDecimal convertToObject(String value, Locale locale)
		{
			return bigDecimalConverter.convertToObject(value, locale).divide(BigDecimal.valueOf(100));
		}

		@Override
		public String convertToString(BigDecimal value, Locale locale)
		{
			return bigDecimalConverter.convertToString(value.multiply(BigDecimal.valueOf(100)), locale);
		}
	}
}
