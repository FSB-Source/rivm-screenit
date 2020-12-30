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

import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.util.StandplaatsPeriodeUtil;
import nl.rivm.screenit.main.web.component.fullcalendar.event.Event;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.MammaCapaciteitOverviewPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.ScreenITEventSourceFactory;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.joda.time.DateTime;

public class StandplaatsEventsProvider extends AbstractScreenITEventProvider
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	private ScreenITEventSourceFactory screenITEventSourceFactory;

	private MammaCapaciteitOverviewPanel capaciteitOverviewPanel;

	public StandplaatsEventsProvider(ScreenITEventSourceFactory screenITEventSourceFactory, MammaCapaciteitOverviewPanel capaciteitOverviewPanel)
	{
		this.capaciteitOverviewPanel = capaciteitOverviewPanel;
		Injector.get().inject(this);
		this.screenITEventSourceFactory = screenITEventSourceFactory;
	}

	@Override
	void createEvents(DateTime start, DateTime end)
	{
		for (PlanningStandplaatsPeriodeDto standplaatsPeriodeDto : screenITEventSourceFactory.getWeekDto().standplaatsPeriodes)
		{
			MammaStandplaats standplaats = hibernateService.get(MammaStandplaats.class, standplaatsPeriodeDto.standplaatsId);

			Event event = new Event();

			capaciteitOverviewPanel.addMeldingTooltip(Model.of(standplaatsPeriodeDto));
			String tooltipId = "tooltip-m" + standplaatsPeriodeDto.conceptId;
			event.setTitle(
				StandplaatsPeriodeUtil.getStandplaatsPeriodeNaam(standplaatsPeriodeDto, standplaats) + "<span class=\"icon-warning-sign "
					+ standplaatsPeriodeDto.meldingenDto.niveau.getCssClass() + "\" data-tooltip=\"" + tooltipId
					+ "\" style=\"margin-left : 5px;\"></span>");
			event.setStart(new DateTime(
				standplaatsPeriodeDto.vanaf.getYear(),
				standplaatsPeriodeDto.vanaf.getMonthValue(),
				standplaatsPeriodeDto.vanaf.getDayOfMonth(), 0, 0));
			event.setEnd(new DateTime(
				standplaatsPeriodeDto.totEnMet.getYear(),
				standplaatsPeriodeDto.totEnMet.getMonthValue(),
				standplaatsPeriodeDto.totEnMet.getDayOfMonth(), 0, 0));
			event.setAllDay(true);
			event.setEditable(false);
			putEvent(standplaatsPeriodeDto.conceptId + "-" + standplaatsPeriodeDto.vanaf.getDayOfWeek().getValue(), event);
		}
	}

	@Override
	ScreenITEventSourceType getSourceType()
	{
		return ScreenITEventSourceType.BLOKKADES;
	}

}
