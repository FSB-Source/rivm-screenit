package nl.rivm.screenit.main.comparator;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Comparator;

import nl.rivm.screenit.main.model.DossierGebeurtenis;

public class DossierGebeurtenisComparator implements Comparator<DossierGebeurtenis>
{
	@Override
	public int compare(DossierGebeurtenis o1, DossierGebeurtenis o2)
	{
		if (o2.getTijd() == null && o1.getTijd() != null)
		{
			return 1;
		}
		if (o2.getTijd() != null && o1.getTijd() == null)
		{
			return -1;
		}
		return o2.getTijd().compareTo(o1.getTijd());
	}

}
