
package nl.rivm.screenit.factory.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;
import java.util.List;
import java.util.Map;

import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.planning.model.IAppointment;
import nl.topicuszorg.planning.model.IParticipant;
import nl.topicuszorg.planning.solvers.model.IDiscipline;
import nl.topicuszorg.wicket.planning.model.Discipline;
import nl.topicuszorg.wicket.planning.model.appointment.Action;
import nl.topicuszorg.wicket.planning.model.appointment.Location;
import nl.topicuszorg.wicket.planning.model.interfaces.IBeschikbaarheid;
import nl.topicuszorg.wicket.planning.model.schedule.ScheduleItem;
import nl.topicuszorg.wicket.planning.util.ISolverResource;

import org.apache.commons.lang.NotImplementedException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SolverResource implements ISolverResource
{

	@Autowired
	private HibernateService hibernateService;

	private static final int opening = 0;

	private static final int sluiten = 23;

	@Override
	public IParticipant getDummyMedewerker(Discipline discipline)
	{

		return new IParticipant()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void setName(String name)
			{

			}

			@Override
			public void setId(Long id)
			{

			}

			@Override
			public void setDescription(String description)
			{

			}

			@Override
			public String getName()
			{
				return null;
			}

			@Override
			public Long getId()
			{
				return null;
			}

			@Override
			public String getDescription()
			{
				return null;
			}

			@Override
			public List<? extends IAppointment> getAppointments(Date startTime, Date endTime)
			{
				return null;
			}
		};
	}

	@Override
	public IParticipant getMedewerkerKloon(Discipline discipline, IParticipant medewerker)
	{
		throw new NotImplementedException();
	}

	@Override
	public List<Discipline> getDisciplines(IParticipant participant)
	{
		return hibernateService.loadAll(Discipline.class);
	}

	@Override
	public List<IBeschikbaarheid> getBeschikbaarheid(ScheduleItem item)
	{
		throw new NotImplementedException();
	}

	@Override
	public IDiscipline getApparaatCategorie(IParticipant apparaat)
	{
		throw new NotImplementedException();
	}

	@Override
	public List<IParticipant> getApparaten(ScheduleItem item)
	{
		throw new NotImplementedException();
	}

	@Override
	public int getSluitingstijd()
	{
		return sluiten;
	}

	@Override
	public int getOpeningstijd()
	{
		return opening;
	}

	@Override
	public List<ScheduleItem> filterLocatieRooster(List<ScheduleItem> items, Action<?> tePlannen)
	{
		throw new NotImplementedException();
	}

	@Override
	public List<IAppointment> getUitsluitingen(List<ScheduleItem> items)
	{
		throw new NotImplementedException();
	}

	@Override
	public Map<Location, List<IAppointment>> getAppointmentsByLocations(Date start, Date end, List<Location> locaties)
	{
		throw new NotImplementedException();
	}

	@Override
	public List<IAppointment> getUitsluitingen(Action<?> action, Date start, Date end)
	{
		throw new NotImplementedException();
	}

}
