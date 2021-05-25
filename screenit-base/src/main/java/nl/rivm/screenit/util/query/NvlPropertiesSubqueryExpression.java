
package nl.rivm.screenit.util.query;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.CriteriaQuery;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.SubqueryExpression;

public class NvlPropertiesSubqueryExpression extends SubqueryExpression
{

	private static final long serialVersionUID = 1L;

	private final String[] propertyNames;

	private String[] nvlObjects;

	public NvlPropertiesSubqueryExpression(String[] propertyNames, String op, DetachedCriteria dc, String[] nvlObjects)
	{
		super(op, null, dc);
		this.propertyNames = propertyNames;
		this.nvlObjects = nvlObjects;
	}

	@Override
	protected String toLeftSqlString(Criteria criteria, CriteriaQuery outerQuery)
	{
		final StringBuilder left = new StringBuilder("(");
		final String[] sqlColumnNames = new String[propertyNames.length];
		for (int i = 0; i < sqlColumnNames.length; ++i)
		{
			sqlColumnNames[i] = outerQuery.getColumn(criteria, propertyNames[i]);
			if (nvlObjects.length > i && nvlObjects[i] != null)
			{
				sqlColumnNames[i] = "coalesce(" + sqlColumnNames[i] + ", " + nvlObjects[i] + ")";
			}
		}
		left.append(StringUtils.join(sqlColumnNames, ", "));
		return left.append(")").toString();
	}

}
