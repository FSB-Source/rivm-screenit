
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

import java.util.ArrayList;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.criterion.CriteriaQuery;
import org.hibernate.criterion.Criterion;
import org.hibernate.engine.spi.TypedValue;
import org.hibernate.internal.util.StringHelper;
import org.hibernate.type.IntegerType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DateYearInExpression implements Criterion
{

	private static final Logger LOG = LoggerFactory.getLogger(DateYearInExpression.class);

	private static final long serialVersionUID = 1L;

	private final String propertyName;

	private final Object[] values;

	public DateYearInExpression(String propertyName, Object[] values)
	{
		if (values != null)
		{
			this.values = values.clone();
		}
		else
		{
			this.values = null;
		}
		this.propertyName = propertyName;
	}

	@Override
	public String toSqlString(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException
	{
		String[] columns = criteriaQuery.getColumnsUsingProjection(criteria, propertyName);
		StringBuffer fragment = new StringBuffer();

		String singleValueParam = StringHelper.repeat("?, ", values.length - 1) + "?";

		fragment.append(" extract(year from ");
		fragment.append(columns[0]);
		fragment.append(" ) in ( ").append(singleValueParam).append(" ) ");

		if (columns.length > 1)
		{
			LOG.warn("multi column fields not supported");
		}
		return fragment.toString();
	}

	@Override
	public TypedValue[] getTypedValues(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException
	{
		ArrayList<TypedValue> list = new ArrayList<TypedValue>();
		for (int j = 0; j < values.length; j++)
		{
			list.add(new TypedValue(IntegerType.INSTANCE, values[j]));
		}
		return list.toArray(new TypedValue[list.size()]);
	}

}
