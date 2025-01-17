package nl.rivm.screenit.util.query;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SQLQueryUtil
{

	private SQLQueryUtil()
	{
	}

	public static String whereOrAnd(String where)
	{
		if (where.equals(""))
		{
			return "WHERE ";
		}
		return where + " AND ";
	}

	public static Map<String, Object> inExpressionParametersEnum(String baseParamName, List<? extends Enum> enums)
	{
		Map<String, Object> inParams = new HashMap<String, Object>();
		for (int i = 0; i < enums.size(); i++)
		{
			String key = baseParamName + i;
			inParams.put(key, enums.get(i).name());
		}
		return inParams;
	}

	public static Map<String, Object> inExpressionParametersLong(String baseParamName, List<Long> longs)
	{
		Map<String, Object> inParams = new HashMap<String, Object>();
		for (int i = 0; i < longs.size(); i++)
		{
			String key = baseParamName + i;
			inParams.put(key, longs.get(i));
		}
		return inParams;
	}
}
