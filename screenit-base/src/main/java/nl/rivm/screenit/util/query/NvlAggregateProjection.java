
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

import java.util.Collections;
import java.util.List;

import org.hibernate.criterion.AggregateProjection;

public class NvlAggregateProjection extends AggregateProjection
{

	private static final long serialVersionUID = 1L;

	private String nvlObject;

	public NvlAggregateProjection(String functionName, String propertyName, String nvlObject)
	{
		super(functionName, propertyName);
		this.nvlObject = nvlObject;
	}

	@Override
	protected List<String> buildFunctionParameterList(String column)
	{
		column = "coalesce(" + column + ", " + nvlObject + ")";
		return Collections.singletonList(column);
	}

}
