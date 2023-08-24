package nl.rivm.screenit.exceptions;

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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.util.DateUtil;

import com.google.common.collect.Range;

public abstract class OpslaanVerwijderenTijdBlokException extends Exception
{
	public OpslaanVerwijderenTijdBlokException(String message)
	{
		super(message);
	}

	public String getAdditionalMessageInfo()
	{
		int i = 0;
		String message = "";
		List<?> items = getItems();
		for (Object item : items)
		{
			Range<Date> range = null;
			if (item instanceof Range)
			{
				range = (Range<Date>) item;
			}
			else
			{
				Object[] roosterItemTijden = (Object[]) item;

				var startDateTimeBestaand = DateUtil.startSeconde((Date) roosterItemTijden[0]);
				var endDateTimeBestaand = DateUtil.startSeconde((Date) roosterItemTijden[1]);
				range = Range.closed(startDateTimeBestaand, endDateTimeBestaand);
			}
			if (i > 0)
			{
				message += ", ";
			}
			message += DateUtil.formatShortDateTime(range.lowerEndpoint()) + "-" + DateUtil.formatTime(range.upperEndpoint());

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
