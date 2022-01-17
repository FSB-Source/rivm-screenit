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

import java.math.BigDecimal;

import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.main.web.component.fullcalendar.event.Event;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.ScreenITEventSourceFactory;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.joda.time.DateTime;

public class WeekCapaciteitEventsProvider extends AbstractScreenITEventProvider
{

	private static final long serialVersionUID = 1L;

	private ScreenITEventSourceFactory screenITEventSourceFactory;

	public WeekCapaciteitEventsProvider(ScreenITEventSourceFactory screenITEventSourceFactory)
	{
		this.screenITEventSourceFactory = screenITEventSourceFactory;
	}

	@Override
	public void createEvents(DateTime start, DateTime end)
	{
		screenITEventSourceFactory.resetCapaciteit(start);
		ScreeningOrganisatie regio = (ScreeningOrganisatie) HibernateHelper
			.deproxy(screenITEventSourceFactory.getScreeningsEenheidModel().getObject().getBeoordelingsEenheid().getParent().getRegio());
		for (PlanningCapaciteitBlokDto blok : screenITEventSourceFactory.getWeekDto().blokken)
		{
			Event event = new Event();
			event.setConceptId(blok.conceptId);

			String title = "";
			String topRight = "";
			event.setBackgroundColor(blok.blokType.getBackgroundColor());
			event.setBorderColor(blok.blokType.getBorderColor());
			switch (blok.blokType)
			{
			case REGULIER:
				topRight = "<span class=\"label pull-right\" style=\"background-color:#540272\">" + blok.aantalOnderzoeken + "</span>";
				break;
			case TEHUIS:
				topRight = "<span class=\"label pull-right\" style=\"background-color:#540272;\">"
					+ BigDecimalUtil.decimalToString(regio.getFactorDubbeleTijdBk().multiply(new BigDecimal(blok.aantalOnderzoeken)))
					+ "</span><span class=\"label pull-right\" style=\"background-color:#8E7B00; margin-right:2px;\">" + blok.aantalOnderzoeken + "</span>";
				break;
			case GEEN_SCREENING:
				title = blok.opmerkingen;
				break;
			}

			boolean inConcept = blok.conceptId != null;

			event.setStart(new DateTime(blok.vanaf));
			event.setEnd(new DateTime(blok.tot));
			event.setTopRight(topRight);
			event.setTitle(title);
			event.setEditable(inConcept);

			putEvent(inConcept ? blok.conceptId.toString() : blok.id.toString(), event);
		}
	}

	@Override
	ScreenITEventSourceType getSourceType()
	{
		return ScreenITEventSourceType.WEEKCAPACITEIT;
	}

}
