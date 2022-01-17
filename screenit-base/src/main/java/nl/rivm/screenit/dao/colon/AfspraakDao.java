
package nl.rivm.screenit.dao.colon;

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

import java.time.LocalDate;
import java.util.Date;
import java.util.EnumSet;
import java.util.List;

import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.planning.AfspraakLocatieWrapper;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.wicket.planning.dao.AppointmentDao;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.Location;

import org.joda.time.Interval;

public interface AfspraakDao extends AppointmentDao
{

	<T extends AbstractAppointment> List<T> getAfspraken(Date start, Date end, T filter, List<Location> locaties, EnumSet<AfspraakStatus> voorAgenda);

	List<ColonIntakeAfspraak> getAfsprakenVoorColoscopiecentrum(WerklijstIntakeFilter zoekObject, ColoscopieCentrum coloscopieCentrum, LocalDate vandaag, long first, long count,
		String property, boolean ascending);

	long countAfsprakenVoorColoscopiecentrum(WerklijstIntakeFilter zoekObject, ColoscopieCentrum coloscopieCentrum, LocalDate vandaag);

	List<Object> getAfsprakenInIntervals(Kamer location, List<Interval> verwijderdeIntervals);

	RoosterItem getRoosterBlokVoorAfspraak(ColonIntakeAfspraak newAfspraak);

	List<AfspraakLocatieWrapper> getAllAfsprakLengtesPerLocatie();

	Date getLaatsteWijzigingsdatumAfspraak(HibernateObject entity);

	List<Object> getRoosterItemsBezetMetAfspraak(Long roosterItemId, Interval currentViewInterval);

	RoosterItem getVrijRoosterBlokVoorAfspraak(ColonIntakeAfspraak newAfspraak);
}
