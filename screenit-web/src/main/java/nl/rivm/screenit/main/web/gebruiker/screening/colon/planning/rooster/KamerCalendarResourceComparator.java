package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.rooster;

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

import nl.rivm.screenit.model.colon.Kamer;
import nl.topicus.wicket.calendar.ICalendarResource;

public class KamerCalendarResourceComparator implements Comparator<ICalendarResource>
{

	@Override
	public int compare(ICalendarResource o1, ICalendarResource o2)
	{
		int cmp = 0;

		if (o1 instanceof Kamer)
		{
			Kamer k1 = (Kamer) o1;
			Kamer k2 = (Kamer) o2;

			cmp += k1.getColoscopieCentrum().getNaam().compareTo(k2.getColoscopieCentrum().getNaam());
			if (cmp == 0)
			{
				cmp = k1.getName().compareTo(k2.getName());
			}
		}

		return cmp;
	}
}
