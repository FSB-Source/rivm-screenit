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

import nl.rivm.screenit.main.web.component.fullcalendar.event.Event;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.ScreenITEventSourceFactory;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.joda.time.DateTime;

public class BlokkadeEventsProvider extends AbstractScreenITEventProvider
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	private ScreenITEventSourceFactory screenITEventSourceFactory;

	public BlokkadeEventsProvider(ScreenITEventSourceFactory screenITEventSourceFactory)
	{
		this.screenITEventSourceFactory = screenITEventSourceFactory;
		Injector.get().inject(this);
	}

	@Override
	void createEvents(DateTime start, DateTime end)
	{
		for (Long blokkadeId : screenITEventSourceFactory.getWeekDto().blokkadesIds)
		{
			String blokkadetitel = null;

			MammaBlokkade blokkade = hibernateService.get(MammaBlokkade.class, blokkadeId);
			switch (blokkade.getType())
			{
			case SCREENINGS_ORGANISATIE:
				blokkadetitel = blokkade.getRegio().getNaam();
				break;
			case SCREENINGS_EENHEID:
				blokkadetitel = blokkade.getScreeningsEenheid().getNaam();
				break;
			case STANDPLAATS:
				blokkadetitel = blokkade.getStandplaats().getNaam();
				break;
			}

			Event event = new Event();
			event.setTitle(blokkadetitel);
			event.setStart(new DateTime(blokkade.getVanaf()));
			event.setEnd(new DateTime(blokkade.getTotEnMet()));
			event.setAllDay(true);
			event.setEditable(false);
			if (StringUtils.isNotBlank(blokkade.getReden()))
			{
				event.setHoverText(blokkade.getReden());
			}
			putEvent(blokkade.getId(), event);
		}
	}

	@Override
	ScreenITEventSourceType getSourceType()
	{
		return ScreenITEventSourceType.BLOKKADES;
	}
}
