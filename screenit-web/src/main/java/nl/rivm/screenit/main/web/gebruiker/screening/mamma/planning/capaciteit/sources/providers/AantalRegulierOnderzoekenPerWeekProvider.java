package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.providers;

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

import nl.rivm.screenit.dto.mamma.planning.PlanningDagDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningWeekDto;
import nl.rivm.screenit.main.web.component.fullcalendar.event.Event;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.ScreenITEventSourceFactory;
import org.joda.time.DateTime;

import java.time.LocalDate;
import java.util.concurrent.atomic.AtomicInteger;

public class AantalRegulierOnderzoekenPerWeekProvider extends AbstractScreenITEventProvider
{

	private static final long serialVersionUID = 1L;

	private ScreenITEventSourceFactory screenITEventSourceFactory;

	public AantalRegulierOnderzoekenPerWeekProvider(ScreenITEventSourceFactory screenITEventSourceFactory)
	{
		this.screenITEventSourceFactory = screenITEventSourceFactory;
	}

	@Override
	void createEvents(DateTime start, DateTime end)
	{
		PlanningWeekDto weekDto = screenITEventSourceFactory.getWeekDto();
		Event event = new Event();
		final int[] totaalVanWeek = { 0 };
		weekDto.dagen.forEach(planningDagDto -> totaalVanWeek[0] += planningDagDto.totaalAantalOnderzoeken);

		event.setTitle("<span class=\"label pull-right\" style=\"background-color:#540272\">" + totaalVanWeek[0] + "</span>");
		event.setAllWeek(true);
		putEvent("totalWeekCapacity", event);
	}

	@Override
	ScreenITEventSourceType getSourceType()
	{
		return ScreenITEventSourceType.AANTAL_REGULIERE_ONDERZOEKEN_PER_WEEK;
	}
}
