package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import org.apache.commons.lang3.StringUtils;

public class EnvironmentUtil
{
	public static Integer getIntegerEnvironmentVariable(String key, Integer defaultValue)
	{
		String property = System.getenv(key);
		return StringUtils.isNotBlank(property) ? Integer.parseInt(property) : defaultValue;
	}

	public static String getStringEnvironmentVariable(String key, String defaultValue)
	{
		String property = System.getenv(key);
		return StringUtils.isNotBlank(property) ? property : defaultValue;
	}

	public static Boolean getBooleanEnvironmentVariable(String key, Boolean defaultValue)
	{
		String property = System.getenv(key);
		return StringUtils.isNotBlank(property) ? Boolean.parseBoolean(property) : defaultValue;
	}
}
