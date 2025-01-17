package nl.rivm.screenit.service.impl;

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

import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Base64;
import java.util.Optional;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import net.fortuna.ical4j.data.CalendarOutputter;
import net.fortuna.ical4j.model.Calendar;
import net.fortuna.ical4j.model.DateTime;
import net.fortuna.ical4j.model.component.VEvent;
import net.fortuna.ical4j.model.property.Attendee;
import net.fortuna.ical4j.model.property.CalScale;
import net.fortuna.ical4j.model.property.Created;
import net.fortuna.ical4j.model.property.Description;
import net.fortuna.ical4j.model.property.DtStamp;
import net.fortuna.ical4j.model.property.Location;
import net.fortuna.ical4j.model.property.Method;
import net.fortuna.ical4j.model.property.ProdId;
import net.fortuna.ical4j.model.property.Sequence;
import net.fortuna.ical4j.model.property.Uid;
import net.fortuna.ical4j.model.property.Version;

import nl.rivm.screenit.service.AfspraakIcalService;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;

@Slf4j
@AllArgsConstructor
@Service
public class AfspraakIcalServiceImpl implements AfspraakIcalService
{
	private final ICurrentDateSupplier currentDateSupplier;

	private static final String ICALHEADER = "-//BVO NL//ScreenIT//NL";

	@Override
	public Optional<String> maakIcalEventEncoded(LocalDateTime startDatum, LocalDateTime eindDatum, String ontvanger, String locatie, String afspraakId, String omschrijving,
		String content)
	{
		try
		{
			var event = maakEventAan(startDatum, eindDatum, ontvanger, locatie, afspraakId, omschrijving, content);

			var calendar = maakCalendarAan(event);

			var calendarOutputter = new CalendarOutputter();
			var outStream = new ByteArrayOutputStream();

			calendarOutputter.output(calendar, outStream);
			var encodedCalendar = Base64.getEncoder().encodeToString(outStream.toByteArray());

			return Optional.of(encodedCalendar);
		}
		catch (Exception e)
		{
			LOG.error(" Er is iets misgegaan met het generen van een ics bestand {}" + e.getMessage());
		}
		return Optional.empty();
	}

	@NotNull
	private VEvent maakEventAan(LocalDateTime startDatum, LocalDateTime eindDatum, String ontvanger, String locatie, String afspraakId, String omschrijving, String content)
	{
		var timeStampStartDatum = createDateTimeUtc(startDatum);
		var timeStampEindDatum = createDateTimeUtc(eindDatum);
		var timeStampNu = createDateTimeUtc(currentDateSupplier.getLocalDateTime());

		var event = new VEvent(timeStampStartDatum, timeStampEindDatum, omschrijving);

		var meetingProperties = event.getProperties();
		meetingProperties.remove(event.getProperty("DTSTAMP")); 
		meetingProperties.add(new DtStamp(createDateTimeUtc(currentDateSupplier.getLocalDateTime())));
		meetingProperties.add(new Uid(afspraakId));
		meetingProperties.add(new Attendee(URI.create(ontvanger)));
		meetingProperties.add(new Location(locatie));
		meetingProperties.add(new Description(content));
		meetingProperties.add(new Sequence(0));
		meetingProperties.add(new Created(timeStampNu));
		return event;
	}

	@NotNull
	private Calendar maakCalendarAan(VEvent event)
	{
		var calendar = new Calendar();
		var calendarProperties = calendar.getProperties();
		calendarProperties.add(new ProdId(ICALHEADER));
		calendarProperties.add(Version.VERSION_2_0);
		calendarProperties.add(CalScale.GREGORIAN);
		calendarProperties.add(Method.PUBLISH);
		calendar.getComponents().add(event);
		return calendar;
	}

	private DateTime createDateTimeUtc(LocalDateTime date)
	{
		DateTime icalDateTime = new DateTime(Timestamp.valueOf(date).getTime());
		icalDateTime.setUtc(true);
		return icalDateTime;
	}
}
