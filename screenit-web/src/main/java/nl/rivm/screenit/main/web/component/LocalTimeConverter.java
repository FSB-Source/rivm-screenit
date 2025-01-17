package nl.rivm.screenit.main.web.component;

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

import java.util.Locale;

import org.apache.commons.beanutils.ConversionException;
import org.apache.commons.lang.StringUtils;
import org.apache.wicket.util.convert.IConverter;
import org.joda.time.LocalTime;

public class LocalTimeConverter implements IConverter<LocalTime>
{

	private static final long serialVersionUID = 1L;

	@Override
	public LocalTime convertToObject(String value, Locale locale)
	{
		if (StringUtils.isNotBlank(value))
		{
			if (value.contains(":"))
			{
				String[] parts = value.split(":");

				if (parts.length == 2 && StringUtils.isNumeric(parts[0]) && StringUtils.isNumeric(parts[1]))
				{
					int hours = Integer.parseInt(parts[0]);
					int mins = Integer.parseInt(parts[1]);
					return new LocalTime(hours, mins);
				}
				else
				{
					throw new ConversionException("Invoer ongeldig, ontvangen invoer: " + value);
				}
			}
			else
			{
				throw new ConversionException("Invoer ongeldig, ontvangen invoer: " + value);
			}
		}

		return null;
	}

	@Override
	public String convertToString(LocalTime value, Locale locale)
	{
		if (value != null)
		{
			return value.toString("HH:mm");
		}

		return null;
	}
}
