package nl.rivm.screenit.main.web.gebruiker.clienten.dossier;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Serializable;
import java.util.Comparator;
import java.util.Date;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;

public class GebeurtenisComparator implements Comparator<ScreeningRondeGebeurtenis>, Serializable
{
	@Override
	public int compare(ScreeningRondeGebeurtenis o1, ScreeningRondeGebeurtenis o2)
	{
		Date datum = o2.getDatum();
		Date datum2 = o1.getDatum();
		if (datum == null)
		{
			datum = new Date();
		}
		if (datum2 == null)
		{
			datum2 = new Date();
		}
		return datum.compareTo(datum2);
	}

}
