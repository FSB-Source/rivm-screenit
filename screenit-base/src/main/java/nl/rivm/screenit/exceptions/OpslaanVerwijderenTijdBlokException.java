package nl.rivm.screenit.exceptions;

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

import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.colon.planning.ColonTijdslot;
import nl.rivm.screenit.util.DateUtil;

import com.google.common.collect.Range;

public abstract class OpslaanVerwijderenTijdBlokException extends Exception
{
	protected OpslaanVerwijderenTijdBlokException(String message)
	{
		super(message);
	}

	public String getAdditionalMessageInfo()
	{
		int i = 0;
		var message = new StringBuilder();
		List<?> items = getItems();
		for (var item : items)
		{
			String startTijd;
			String eindTijd;
			if (item instanceof Range)
			{
				var range = (Range<?>) item;
				var lowerEndpoint = range.lowerEndpoint();
				if (lowerEndpoint instanceof Date)
				{
					startTijd = DateUtil.formatShortDateTime((Date) lowerEndpoint);
					eindTijd = DateUtil.formatTime((Date) range.upperEndpoint());
				}
				else
				{
					var localDateTime = (LocalDateTime) lowerEndpoint;
					startTijd = DateUtil.formatShortDateTime(localDateTime);
					eindTijd = DateUtil.formatTime(DateUtil.toUtilDate((LocalDateTime) range.upperEndpoint()));
				}
			}
			else if (item instanceof ColonTijdslot)
			{
				var tijdslot = (ColonTijdslot) item;
				startTijd = DateUtil.formatShortDateTime(tijdslot.getVanaf());
				eindTijd = DateUtil.formatLocalTime(tijdslot.getTot());
			}
			else
			{
				Object[] afspraakslotTijden = (Object[]) item;

				var startDateTimeBestaand = DateUtil.startMinuut((Date) afspraakslotTijden[0]);
				startTijd = DateUtil.formatShortDateTime(startDateTimeBestaand);
				var endDateTimeBestaand = DateUtil.startMinuut((Date) afspraakslotTijden[1]);
				eindTijd = DateUtil.formatTime(endDateTimeBestaand);
			}
			if (i > 0)
			{
				message.append(", ");
			}
			message.append(startTijd).append("-").append(eindTijd);

			i++;
			if (i == 5)
			{
				message.append(" en ").append(items.size() - 5).append(" andere..");
				break;
			}
		}
		message.append(".");
		return message.toString();
	}

	protected abstract List<?> getItems();
}
