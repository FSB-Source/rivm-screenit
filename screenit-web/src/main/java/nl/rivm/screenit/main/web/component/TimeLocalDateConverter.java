package nl.rivm.screenit.main.web.component;

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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Locale;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.util.convert.IConverter;

public class TimeLocalDateConverter implements IConverter<LocalDate>
{

	private static final long serialVersionUID = 1L;

	@Override
	public LocalDate convertToObject(String value, Locale locale)
	{
		if (StringUtils.isNotBlank(value))
		{
			Date convertToObject = new MultiDateConverter().convertToObject(value, locale);
			if (convertToObject != null)
			{
				return DateUtil.toLocalDate(convertToObject);
			}
		}

		return null;
	}

	@Override
	public String convertToString(LocalDate value, Locale locale)
	{
		if (value != null)
		{
			return DateTimeFormatter.ofPattern(Constants.DEFAULT_DATE_FORMAT).format(value);
		}

		return null;
	}
}
