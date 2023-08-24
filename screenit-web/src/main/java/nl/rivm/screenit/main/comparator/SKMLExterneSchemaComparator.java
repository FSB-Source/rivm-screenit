package nl.rivm.screenit.main.comparator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Comparator;

import nl.rivm.screenit.model.colon.SKMLExternSchema;

public class SKMLExterneSchemaComparator implements Comparator<SKMLExternSchema>
{
	@Override
	public int compare(SKMLExternSchema o1, SKMLExternSchema o2)
	{
		if (o1 != null && o1.getDeadline() != null && o2 != null && o2.getDeadline() != null)
		{
			return o1.getDeadline().compareTo(o2.getDeadline());
		}
		else if (o1 == null || o1.getDeadline() == null && o2 != null && o2.getDeadline() != null)
		{
			return 1;
		}
		else if (o2 == null || o2.getDeadline() == null && o1 != null && o1.getDeadline() != null)
		{
			return -1;
		}
		else
		{
			return 0;
		}
	}
}
