
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

public class DateRestrictions
{

	private DateRestrictions()
	{
	}

	public static DateExpression eq(String propertyName, Object value)
	{
		return new DateExpression(propertyName, value, "=");
	}

	public static DateExpression gt(String propertyName, Object value)
	{
		return new DateExpression(propertyName, value, ">");
	}

	public static DateExpression lt(String propertyName, Object value)
	{
		return new DateExpression(propertyName, value, "<");
	}

	public static DateExpression le(String propertyName, Object value)
	{
		return new DateExpression(propertyName, value, "<=");
	}

	public static DateExpression ge(String propertyName, Object value)
	{
		return new DateExpression(propertyName, value, ">=");
	}

	public static DateExpression ne(String propertyName, Object value)
	{
		return new DateExpression(propertyName, value, "<>");
	}

	public static DatePropertyExpression eqProperty(String propertyName, String otherPropertyName)
	{
		return new DatePropertyExpression(propertyName, otherPropertyName, "=");
	}

	public static DatePropertyExpression leProperty(String propertyName, String otherPropertyName)
	{
		return new DatePropertyExpression(propertyName, otherPropertyName, "<=");
	}

	public static DatePropertyExpression geProperty(String propertyName, String otherPropertyName)
	{
		return new DatePropertyExpression(propertyName, otherPropertyName, ">=");
	}

	public static DatePropertyExpression ltProperty(String propertyName, String otherPropertyName)
	{
		return new DatePropertyExpression(propertyName, otherPropertyName, "<");
	}

	public static DatePropertyExpression gtProperty(String propertyName, String otherPropertyName)
	{
		return new DatePropertyExpression(propertyName, otherPropertyName, ">");
	}
}
