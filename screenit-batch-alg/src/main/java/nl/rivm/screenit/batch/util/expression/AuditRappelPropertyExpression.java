package nl.rivm.screenit.batch.util.expression;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.criterion.CriteriaQuery;
import org.hibernate.criterion.PropertyExpression;

public class AuditRappelPropertyExpression extends PropertyExpression
{

	private static final long serialVersionUID = 1L;

	private final String propertyName;

	private final String otherPropertyName;

	public AuditRappelPropertyExpression(String propertyName, String otherPropertyName, String op)
	{
		super(propertyName, otherPropertyName, op);
		this.propertyName = propertyName;
		this.otherPropertyName = otherPropertyName;
	}

	@Override
	public String toSqlString(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException
	{
		String[] xcols = criteriaQuery.findColumns(propertyName, criteria);
		String[] ycols = criteriaQuery.findColumns(otherPropertyName, criteria);

		String[] strings = { "to_date(to_char(" + xcols[0] + ", 'YYYY/MM/DD'), 'YYYY/MM/DD') " + getOp() + " (current_date - interval '1' day * " + ycols[0] + ") " };

		String result = String.join(" and ", strings);
		if (xcols.length > 1)
		{
			result = '(' + result + ')';
		}
		return result;
	}
}
