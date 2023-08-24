package nl.rivm.screenit.util.query;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

public class StringLengthRestrictions
{

    public static StringLengthPropertyExpression eq(String propertyName, String otherPropertyName)
    {
        return new StringLengthPropertyExpression(propertyName, otherPropertyName, "=");
    }

    public static StringLengthPropertyExpression le(String propertyName, String otherPropertyName)
    {
        return new StringLengthPropertyExpression(propertyName, otherPropertyName, "<=");
    }

    public static StringLengthPropertyExpression lt(String propertyName, String otherPropertyName)
    {
        return new StringLengthPropertyExpression(propertyName, otherPropertyName, "<");
    }

    public static StringLengthPropertyExpression ge(String propertyName, String otherPropertyName)
    {
        return new StringLengthPropertyExpression(propertyName, otherPropertyName, ">=");
    }

    public static StringLengthPropertyExpression gt(String propertyName, String otherPropertyName)
    {
        return new StringLengthPropertyExpression(propertyName, otherPropertyName, ">");
    }

}
