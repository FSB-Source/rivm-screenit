package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.providers;

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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.main.web.component.fullcalendar.event.Event;
import nl.rivm.screenit.main.web.component.fullcalendar.event.EventNotFoundException;
import nl.rivm.screenit.main.web.component.fullcalendar.event.EventProvider;

import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;

public abstract class AbstractScreenITEventProvider implements EventProvider
{

	private static final long serialVersionUID = 1L;

	private Map<String, Event> events = new HashMap<>();

	@Override
	public final Collection<Event> getEvents(DateTime start, DateTime end)
	{
		events.clear();
		createEvents(start, end);
		return events.values();
	}

	void putEvent(Number id, Event event)
	{
		putEvent(id + "", event);
	}

	void putEvent(String id, Event event)
	{
		event.setId(getSourceType().getPrefix() + id);
		events.put(id, event);
	}

	@Override
	public final Event getEventForId(String id) throws EventNotFoundException
	{
		Event event = events.get(ScreenITEventSourceType.getEventId(getSourceType(), id));
		if (event != null)
		{
			return event;
		}
		throw new EventNotFoundException("Event with id: " + id
			+ " not found");
	}

	abstract void createEvents(DateTime start, DateTime end);

	abstract ScreenITEventSourceType getSourceType();

	public enum ScreenITEventSourceType
	{

		WEEKCAPACITEIT("week-capaciteit-"),

		BLOKKADES("blokkades-"),

		STANDPLAATS("standplaats-"),

		AANTAL_REGULIERE_ONDERZOEKEN_PER_DAG("aantal-onderzoeken-"),

		AANTAL_REGULIERE_ONDERZOEKEN_PER_WEEK("aantal-onderzoeken-per-week-");

		private String prefix;

		ScreenITEventSourceType(String prefix)
		{
			this.prefix = prefix;
		}

		public String getPrefix()
		{
			return prefix;
		}

		public static ScreenITEventSourceType getSourceTypeForEvent(String eventId)
		{
			if (StringUtils.startsWith(eventId, WEEKCAPACITEIT.getPrefix()))
			{
				return WEEKCAPACITEIT;
			}
			else if (StringUtils.startsWith(eventId, BLOKKADES.getPrefix()))
			{
				return BLOKKADES;
			}
			else if (StringUtils.startsWith(eventId, STANDPLAATS.getPrefix()))
			{
				return STANDPLAATS;
			}
			else if (StringUtils.startsWith(eventId, AANTAL_REGULIERE_ONDERZOEKEN_PER_DAG.getPrefix()))
			{
				return AANTAL_REGULIERE_ONDERZOEKEN_PER_DAG;
			}
			else if (StringUtils.startsWith(eventId, AANTAL_REGULIERE_ONDERZOEKEN_PER_WEEK.getPrefix()))
			{
				return AANTAL_REGULIERE_ONDERZOEKEN_PER_WEEK;
			}
			throw new IllegalStateException();
		}

		public static String getEventId(ScreenITEventSourceType eventSourceType, String id)
		{
			return StringUtils.removeStart(id, eventSourceType.getPrefix());
		}
	}
}
