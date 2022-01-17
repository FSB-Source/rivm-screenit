
package nl.rivm.screenit.exceptions;

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

import java.util.List;

import org.joda.time.DateTime;
import org.joda.time.Interval;

public abstract class OpslaanVerwijderenTijdBlokException extends Exception
{

	private static final long serialVersionUID = 1L;

	public OpslaanVerwijderenTijdBlokException(String message)
	{
		super(message);
	}

	public String getAdditionalMessageInfo()
	{
		int i = 0;
		String message = " ";
		List<?> items = getItems();
		for (Object item : items)
		{
			Interval interval = null;
			if (item instanceof Interval)
			{
				interval = (Interval) item;
			}
			else
			{
				Object[] roosterItemTijden = (Object[]) item;
				DateTime startDateTimeBestaand = new DateTime(roosterItemTijden[0]).withMillisOfSecond(0);
				DateTime endDateTimeBestaand = new DateTime(roosterItemTijden[1]).withMillisOfSecond(0);
				interval = new Interval(startDateTimeBestaand, endDateTimeBestaand);
			}
			if (i > 0)
			{
				message += ", ";
			}
			message += interval.getStart().toString("dd-MM-yyyy HH:mm") + "-" + interval.getEnd().toString("HH:mm");

			i++;
			if (i == 5)
			{
				message += " en " + (items.size() - 5) + " andere..";
				break;
			}
		}
		message += ".";
		return message;
	}

	protected abstract List<?> getItems();
}
