package nl.rivm.screenit.main.web.gebruiker.screening.colon.planning.rooster;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.model.RecurrenceOption;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.planning.model.IAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;

import org.joda.time.DateTime;

public class CalendarRefresher<T extends AbstractAppointment>
{

	private final boolean verwijderd;

	private final List<T> aangepast;

	private final RecurrenceOption recurrenceOption;

	private Date aangepastTot;

	public CalendarRefresher(boolean verwijderd, T aangepast, RecurrenceOption recurrenceOption)
	{
		this(verwijderd, Collections.singletonList(aangepast), recurrenceOption);
	}

	public CalendarRefresher(boolean verwijderd, List<T> aangepasteTijdSloten, RecurrenceOption recurrenceOption)
	{
		this.verwijderd = verwijderd;
		this.aangepast = aangepasteTijdSloten;
		this.recurrenceOption = recurrenceOption;

	}

	public boolean isVerwijderd()
	{
		return verwijderd;
	}

	public List<T> getAangepast()
	{
		return aangepast;
	}

	public RecurrenceOption getRecurrenceOption()
	{
		return recurrenceOption;
	}

	public Date getAangepastTot()
	{
		return aangepastTot;
	}

	public void setAangepastTot(Date aangepastTot)
	{
		this.aangepastTot = aangepastTot;
	}

	public List<T> getAfspraken()
	{
		List<T> afspraken = new ArrayList<T>();
		afspraken.addAll(getAangepast());
		for (T aangepast : getAangepast())
		{
			Date end = new DateTime(aangepast.getStartTime()).plusWeeks(1).toDate();
			Date start = aangepast.getStartTime();

			if (RecurrenceOption.WIJZIG_OF_VERWIJDER_TOT.equals(recurrenceOption) && aangepastTot != null && aangepastTot.before(end))
			{
				end = aangepastTot;
			}
			if (RecurrenceOption.WIJZIG_OF_VERWIJDER_HELE_SERIE.equals(recurrenceOption))
			{
				start = new DateTime(start).plusWeeks(-1).toDate();
			}
			else if (!verwijderd)
			{
				end = DateUtil.eindDag(end);
			}

			if (aangepast.getRecurrence() != null)
			{
				for (IAppointment appointment : aangepast.getRecurrence().getAppointments())
				{
					@SuppressWarnings("unchecked")
					T next = (T) appointment;
					if (next.getStartTime().after(start) && next.getEndTime().before(end))
					{
						afspraken.add(next);
					}
				}
			}
		}
		return afspraken;
	}
}
