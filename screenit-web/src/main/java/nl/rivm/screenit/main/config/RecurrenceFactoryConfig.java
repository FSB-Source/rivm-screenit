package nl.rivm.screenit.main.config;

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

import java.util.HashMap;

import nl.topicus.wicket.calendar.RecurrenceFactory;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class RecurrenceFactoryConfig
{

	@Bean
	public RecurrenceFactory recurrenceFactory()
	{
		var factory = new RecurrenceFactory();
		var recurrences = new HashMap<String, String>();
		recurrences.put("Geen", "nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence");
		recurrences.put("Dagelijks", "nl.topicuszorg.wicket.planning.model.appointment.recurrence.DailyRecurrence");
		recurrences.put("Wekelijks", "nl.topicuszorg.wicket.planning.model.appointment.recurrence.WeeklyRecurrence");
		recurrences.put("Maandelijks", "nl.topicuszorg.wicket.planning.model.appointment.recurrence.MonthlyRecurrence");
		recurrences.put("Jaarlijks", "nl.topicuszorg.wicket.planning.model.appointment.recurrence.YearlyRecurrence");
		factory.setRecurrences(recurrences);
		return factory;
	}

}
