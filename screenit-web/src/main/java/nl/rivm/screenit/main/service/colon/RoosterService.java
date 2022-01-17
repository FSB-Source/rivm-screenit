package nl.rivm.screenit.main.service.colon;

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

import java.util.Date;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.dao.colon.IntakelocatieVanTotEnMetFilter;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.model.RecurrenceOption;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemListViewWrapper;
import nl.rivm.screenit.model.colon.RoosterItemStatus;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.util.Periode;

import org.joda.time.Interval;

public interface RoosterService
{

	List<RoosterItem> getRooster(Periode periode, List<Kamer> kamers);

	List<RoosterItem> getOneindigeRoostersItems();

	void magRoosterItemOpslaanVerwijderen(RoosterItem roosteritem, RecurrenceOption recurrenceOption, Date recurrenceEditEnd,
		Date origRecEndDateTime, boolean wijzigen) throws OpslaanVerwijderenTijdBlokException;

	void magBlokkadeOpslaanVerwijderen(ColonBlokkade blokkade, RecurrenceOption recurrenceOption, Date recurrenceEditEnd, Date origRecEndDateTime, boolean wijzigen,
		List<Kamer> kamers) throws OpslaanVerwijderenTijdBlokException;

	void toevoegenHerhaling(AbstractAppointment appointment);

	Iterator<RoosterItemListViewWrapper> getRoosterBlokken(String sortProperty, boolean asc, long first, long count, RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie);

	long getRoosterBlokkenCount(RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie);

	RoosterItemStatus getRoosterItemStatus(RoosterItem roosterItem);

	Integer getCurrentAantalRoosterBlokken(ColoscopieCentrum intakeLocatie, Interval periode);

	List<Date> getMdlDatums(Client client, IntakelocatieVanTotEnMetFilter intakeVanTotEnMetFilter);

	List<ColonBlokkade> getBlokkades(Periode periode, List<Kamer> kamers);

	List<ColonBlokkade> getBlokkades(String sortProperty, boolean ascending, long first, long count, RoosterListViewFilter filter, ColoscopieCentrum intakelocatie);

	long getBlokkadesCount(RoosterListViewFilter filter, ColoscopieCentrum intakelocatie);

}
