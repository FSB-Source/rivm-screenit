package nl.rivm.screenit.factory.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.Afspraak;
import nl.topicuszorg.planning.model.IParticipant;
import nl.topicuszorg.planning.solvers.model.IAppointmentFactory;
import nl.topicuszorg.wicket.planning.model.Discipline;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;

import org.springframework.stereotype.Component;

@Component
public class AppointmentFactory implements IAppointmentFactory<AbstractAppointment, AbstractAppointment>
{

	public AppointmentFactory()
	{
	}

	@Override
	public Afspraak newAppointment(AbstractAppointment geplandeAfspraak)
	{
		Afspraak actie = new Afspraak();

		actie.setStartTime(geplandeAfspraak.getStartTime());
		actie.setEndTime(geplandeAfspraak.getEndTime());
		actie.setLocation(geplandeAfspraak.getLocation());

		if (geplandeAfspraak instanceof Afspraak)
		{
			actie.setDisciplines(new ArrayList<Discipline>(((Afspraak) geplandeAfspraak).getDisciplines()));
		}

		return actie;
	}

	@Override
	public void setAppointmentValues(AbstractAppointment geplandeAfspraak, Date start, Date end, List<? extends IParticipant> participants)
	{
		geplandeAfspraak.setStartTime(start);
		geplandeAfspraak.setEndTime(end);
	}

}
