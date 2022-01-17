package nl.rivm.screenit.util.query;

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

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.criterion.CriteriaQuery;
import org.hibernate.criterion.Criterion;
import org.hibernate.engine.spi.TypedValue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DatePropertyExpression implements Criterion
{
	private static final TypedValue[] NO_TYPED_VALUES = new TypedValue[0];

	private static final Logger LOG = LoggerFactory.getLogger(DatePropertyExpression.class);

	private final String propertyName;

	private final String otherPropertyName;

	private final String operation;

	protected DatePropertyExpression(String propertyName, String otherPropertyName, String operation)
	{
		this.propertyName = propertyName;
		this.otherPropertyName = otherPropertyName;
		this.operation = operation;

	}

	@Override
	public String toSqlString(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException
	{
		final String[] lhsColumns = criteriaQuery.findColumns( propertyName, criteria );
		final String[] rhsColumns = criteriaQuery.findColumns( otherPropertyName, criteria );
		StringBuffer fragment = new StringBuffer();

		fragment.append(" date_trunc('day', ");
		fragment.append(lhsColumns[0]);
		fragment.append(" ) ");
		fragment.append(operation);
		fragment.append(" date_trunc('day', ");
		fragment.append(rhsColumns[0]);
		fragment.append(" ) ");

		if (lhsColumns.length > 1)
		{
			LOG.warn("multi column fields not supported");
		}
		return fragment.toString();
	}

	@Override
	public TypedValue[] getTypedValues(Criteria criteria, CriteriaQuery criteriaQuery)
	{
		return NO_TYPED_VALUES;
	}

	@Override
	public String toString()
	{
		return propertyName + getOperation() + otherPropertyName;
	}

	public String getOperation()
	{
		return operation;
	}
}
