package nl.rivm.screenit.util.query;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.criterion.CriteriaQuery;
import org.hibernate.criterion.PropertyExpression;

public class StringLengthPropertyExpression extends PropertyExpression
{

	private final String _propertyName;
	private final String _otherPropertyName;

	public StringLengthPropertyExpression(String propertyName, String otherPropertyName, String op)
	{
		super(propertyName, otherPropertyName, op);
		_propertyName = propertyName;
		_otherPropertyName = otherPropertyName;
	}

	@Override
	public String toSqlString(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException
	{
		final String[] lhsColumns = criteriaQuery.findColumns( _propertyName, criteria );
		final String[] rhsColumns = criteriaQuery.findColumns( _otherPropertyName, criteria );

		if (lhsColumns.length == 1 && rhsColumns.length == 1)
		{
			return lhsColumns[0] + getOp() + " length(" + rhsColumns[0] + ")";
		}
		throw new HibernateException("Too many or too few properties given. Specify exactly 2 properties!");
	}

}
