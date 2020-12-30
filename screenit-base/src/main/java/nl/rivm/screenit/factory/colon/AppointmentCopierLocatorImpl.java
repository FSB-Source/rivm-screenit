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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.topicuszorg.planning.model.abstractimpl.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.schedule.ScheduleItem;
import nl.topicuszorg.wicket.planning.util.AppointmentCopier;
import nl.topicuszorg.wicket.planning.util.AppointmentCopierLocator;
import nl.topicuszorg.wicket.planning.util.IAppointmentCallback;

import org.springframework.stereotype.Component;

@Component
public class AppointmentCopierLocatorImpl implements AppointmentCopierLocator
{

	@SuppressWarnings("unchecked")
	@Override
	public <T extends AbstractAppointment> AppointmentCopier<T> getCopier(Class<? extends T> clazz)
	{
		AppointmentCopier<T> copier = null;

		if (RoosterItem.class.isAssignableFrom(clazz))
		{
			copier = (AppointmentCopier<T>) new RoosterItemCopier();
		}
		if (ColonBlokkade.class.isAssignableFrom(clazz))
		{
			copier = (AppointmentCopier<T>) new ColonBlokkadeCopier();
		}
		return copier;
	}

	private class RoosterItemCopier implements AppointmentCopier<RoosterItem>
	{

		@Override
		public List<? extends ScheduleItem> getRoosterblokken(AbstractRecurrence recurrence)
		{
			return new ArrayList<ScheduleItem>();
		}

		@Override
		public void ontkoppelMedewerker(nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment current)
		{
		}

		@Override
		public boolean nietPlannenOpMeerdereKamers(nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment clone, ScheduleItem scheduleItem)
		{
			return false;
		}

		@Override
		public void copy(RoosterItem template, RoosterItem target, IAppointmentCallback callback,
			List<? extends nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment> beschikbaarheid)
		{
			target.setScheduleSet(template.getScheduleSet());
		}

		@Override
		public void bewerkCallbackVoorAppointment(nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment clone, IAppointmentCallback callBackObject)
		{

		}

		@Override
		public Map<Date, List<? extends nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment>> getBeschikbaarheidPerDag(AbstractRecurrence recurrence,
			AbstractAppointment template)
		{
			return new HashMap<Date, List<? extends nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment>>();
		}
	}

	private class ColonBlokkadeCopier implements AppointmentCopier<ColonBlokkade>
	{

		@Override
		public List<? extends ScheduleItem> getRoosterblokken(AbstractRecurrence recurrence)
		{
			return new ArrayList<ScheduleItem>();
		}

		@Override
		public void ontkoppelMedewerker(nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment current)
		{
		}

		@Override
		public boolean nietPlannenOpMeerdereKamers(nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment clone, ScheduleItem scheduleItem)
		{
			return false;
		}

		@Override
		public void copy(ColonBlokkade template, ColonBlokkade target, IAppointmentCallback callback,
			List<? extends nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment> beschikbaarheid)
		{

		}

		@Override
		public void bewerkCallbackVoorAppointment(nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment clone, IAppointmentCallback callBackObject)
		{

		}

		@Override
		public Map<Date, List<? extends nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment>> getBeschikbaarheidPerDag(AbstractRecurrence recurrence,
			AbstractAppointment template)
		{
			return new HashMap<Date, List<? extends nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment>>();
		}
	}

}
