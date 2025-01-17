package nl.rivm.screenit.main.web.component.form;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.component.validator.BigDecimalScaleValidator;

import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.util.convert.IConverter;
import org.apache.wicket.validation.validator.RangeValidator;

public class BigDecimalField extends TextField<BigDecimal>
{

	private static final long serialVersionUID = 1L;

	public BigDecimalField(String id)
	{
		super(id);
		add(new BigDecimalScaleValidator());
		add(RangeValidator.range(BigDecimal.ZERO, new BigDecimal(100)));
	}

	public BigDecimalField(String id, int scale)
	{
		this(id, scale, BigDecimal.ZERO, BigDecimal.valueOf(100));
	}

	public BigDecimalField(String id, int scale, BigDecimal minVal, BigDecimal maxVal)
	{
		super(id);
		add(new BigDecimalScaleValidator(scale));
		add(RangeValidator.range(minVal, maxVal));
	}

	@Override
	public <C> IConverter<C> getConverter(Class<C> type)
	{
		return (IConverter<C>) new ScreenITScaledBigDecimalConverter();
	}

}
