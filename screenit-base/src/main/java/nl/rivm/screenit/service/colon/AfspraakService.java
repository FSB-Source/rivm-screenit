
package nl.rivm.screenit.service.colon;

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

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afspraak;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.NieuweIntakeAfspraakMakenReden;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.enums.BriefType;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.planning.model.IAppointment;
import nl.topicuszorg.wicket.planning.model.appointment.Location;
import nl.topicuszorg.wicket.planning.services.AppointmentService;

import org.joda.time.DateTime;
import org.joda.time.Interval;

public interface AfspraakService extends AppointmentService
{

	List<? extends IAppointment> getAppointments(Location locatie, Date start, Date end, boolean showSchedule);

	List<Afspraak> getAppointments(Client client);

	void verplaatsAfspraak(ColonIntakeAfspraak nieuweAfspraak, Account account, BriefType briefType, boolean briefTegenhouden, boolean uitRooster);

	void annuleerAfspraak(Afspraak afspraak, Account account, AfspraakStatus status, boolean briefTegenhouden);

	void maakNieuweAfspraak(Client client, NieuweIntakeAfspraakMakenReden reden, ColonIntakeAfspraak nieuweAfspraak, boolean briefTegenhouden, boolean uitRooster,
		BriefType briefType, Account account);

	List<Afspraak> getHistorischeAppointments(Client client);

	List<ColonIntakeAfspraak> getAfsprakenVoorColoscopiecentrum(WerklijstIntakeFilter zoekObject, ColoscopieCentrum coloscopieCentrum, long first, long count,
		String property, boolean ascending);

	long countAfsprakenVoorColoscopiecentrum(WerklijstIntakeFilter zoekObject, ColoscopieCentrum coloscopieCentrum);

	void verzendHuisartsBerichtOpnieuw(Client client, Account account);

	boolean magWijzigenAfzeggen(Afspraak afspraak);

	List<Object> getAfsprakenKamersInIntervals(Kamer location, List<Interval> verwijderdeIntervals);

	RoosterItem getRoosterBlokVoorAfspraak(ColonIntakeAfspraak newAfspraak);

	RoosterItem getVrijRoosterBlokVoorAfspraak(ColonIntakeAfspraak newAfspraak);

	void setAfspraakStatus(Afspraak afspraak, AfspraakStatus status);

	Date getLaatsteWijzigingsdatumAfspraak(HibernateObject entity);

	void afspraakAfzeggen(ColonIntakeAfspraak afspraak, AfspraakStatus status, DateTime nu, boolean briefTegenhouden);

	List<Object> getRoosterItemsBezetMetAfspraak(Long roosterItemId, Interval currentViewInterval);

}
