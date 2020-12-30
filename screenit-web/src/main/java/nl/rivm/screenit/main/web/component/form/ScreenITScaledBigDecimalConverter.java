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

import java.math.BigDecimal;
import java.util.Locale;

import nl.topicuszorg.wicket.input.converters.ScaledBigDecimalConverter;

import org.apache.commons.lang.StringUtils;

public class ScreenITScaledBigDecimalConverter extends ScaledBigDecimalConverter
{

	@Override
	public BigDecimal convertToObject(String value, Locale locale)
	{
		value = value.replaceAll(",", ".");
		if (value.indexOf('.') != value.lastIndexOf('.'))
		{
			throw newConversionException("Meer dan één komma of punt", value, locale);
		}
		return StringUtils.isBlank(value) ? null : this.parse(value, null, null, locale);
	}
}
