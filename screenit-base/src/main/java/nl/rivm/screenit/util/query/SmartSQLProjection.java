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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.criterion.CriteriaQuery;
import org.hibernate.criterion.SQLProjection;
import org.hibernate.type.Type;

public class SmartSQLProjection extends SQLProjection
{

	private static final Pattern ALIAS_INTERPOLATE_PATTERN = Pattern.compile("\\{(.*?)}");

	private String sql;

	private String groupBy;

	public SmartSQLProjection(String sql, String[] columnAliases, Type[] types)
	{
		this(sql, null, columnAliases, types);
	}

	public SmartSQLProjection(String sql, String groupBy, String[] columnAliases, Type[] types)
	{
		super(sql, groupBy, columnAliases, types);
		this.sql = sql;
		this.groupBy = groupBy;
	}

	@Override
	public String toSqlString(Criteria criteria, int loc, CriteriaQuery criteriaQuery) throws HibernateException
	{
		return replaceAliasWithSQLAlias(sql, criteria, criteriaQuery);
	}

	@Override
	public String toGroupSqlString(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException
	{
		return replaceAliasWithSQLAlias(groupBy, criteria, criteriaQuery);
	}

	private String replaceAliasWithSQLAlias(String queryString, Criteria criteria, CriteriaQuery criteriaQuery)
	{

		String sqlString = queryString.replace("{alias}", criteriaQuery.getSQLAlias(criteria));

		Matcher matchPattern = ALIAS_INTERPOLATE_PATTERN.matcher(sqlString);
		while (matchPattern.find())
		{
			String alias = matchPattern.group(1);
			String sqlAlias = criteriaQuery.getSQLAlias(criteria, (alias + ".")); 
			if (sqlAlias != null)
			{
				sqlString = sqlString.replace("{" + alias + "}", sqlAlias);
			}
		}

		return sqlString;
	}

}
