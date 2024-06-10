package nl.rivm.screenit.util.query;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.ArrayList;

import lombok.extern.slf4j.Slf4j;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.criterion.CriteriaQuery;
import org.hibernate.criterion.Criterion;
import org.hibernate.engine.spi.TypedValue;
import org.hibernate.type.DateType;

@Slf4j
public class DateExpression implements Criterion
{

	private final String propertyName;

	private final Object value;

	private final String operator;

	public DateExpression(String propertyName, Object value, String operator)
	{
		this.propertyName = propertyName;
		this.value = value;
		this.operator = operator;
	}

	@Override
	public String toSqlString(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException
	{
		var fragment = new StringBuffer();

		var columns = criteriaQuery.getColumnsUsingProjection(criteria, propertyName);

		fragment.append(" date_trunc('day', ");
		fragment.append(columns[0]);
		fragment.append(" ) ");
		fragment.append(operator);
		fragment.append(" ? ");

		if (columns.length > 1)
		{
			LOG.warn("multi column fields not supported");
		}
		return fragment.toString();
	}

	@Override
	public TypedValue[] getTypedValues(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException
	{
		var list = new ArrayList<TypedValue>();
		list.add(new TypedValue(DateType.INSTANCE, value));
		return list.toArray(new TypedValue[list.size()]);
	}

}
