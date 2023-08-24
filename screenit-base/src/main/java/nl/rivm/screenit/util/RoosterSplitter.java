package nl.rivm.screenit.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.AbstractRecurrence;
import nl.topicuszorg.wicket.planning.model.appointment.recurrence.NoRecurrence;

import org.hibernate.Hibernate;
import org.hibernate.Session;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class RoosterSplitter
{

	public static void splitRoosterBlok(Object session, RoosterItem unsavedObject)
	{
		var startDate = DateUtil.toLocalDateTime(unsavedObject.getStartTime());
		int diffStartEndMinutes = Math.abs(DateUtil.getPeriodeTussenTweeDatums(startDate, DateUtil.toLocalDateTime(unsavedObject.getEndTime()), ChronoUnit.MINUTES));

		Integer duurAfspraakInMinuten = unsavedObject.getLocation().getColoscopieCentrum().getAfspraakDefinities().get(0).getDuurAfspraakInMinuten();
		int berekendAantalBlokken = diffStartEndMinutes / duurAfspraakInMinuten;
		LocalDateTime endTime;
		for (int i = 0; i < berekendAantalBlokken; i++)
		{
			RoosterItem splittedRoosterBlok = unsavedObject.transientClone();
			AbstractRecurrence recurrence = unsavedObject.getRecurrence();
			if (recurrence != null && !NoRecurrence.class.isAssignableFrom(Hibernate.getClass(recurrence)))
			{
				AbstractRecurrence clonedRecurrence = recurrence.transientClone();
				clonedRecurrence.setFirstAppointment(splittedRoosterBlok);
				splittedRoosterBlok.setRecurrence(clonedRecurrence);
			}
			splittedRoosterBlok.setStartTime(DateUtil.toUtilDate(startDate));
			endTime = startDate.plusMinutes(duurAfspraakInMinuten);
			splittedRoosterBlok.setEndTime(DateUtil.toUtilDate(endTime));
			startDate = endTime;
			if (session instanceof Session)
			{
				((Session) session).saveOrUpdate(splittedRoosterBlok);
			}
			else if (session instanceof HibernateService)
			{
				((HibernateService) session).saveOrUpdate(splittedRoosterBlok);
			}
		}
	}

}
