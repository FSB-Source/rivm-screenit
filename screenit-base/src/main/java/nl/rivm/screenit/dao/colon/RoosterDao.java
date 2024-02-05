package nl.rivm.screenit.dao.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.RoosterItemListViewWrapper;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.RoosterItem;
import nl.rivm.screenit.model.colon.planning.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.planning.VrijSlotZonderKamerFilter;
import nl.topicuszorg.wicket.planning.model.appointment.AbstractAppointment;
import nl.topicuszorg.wicket.planning.util.Periode;

import com.google.common.collect.Range;

public interface RoosterDao
{

	<T extends AbstractAppointment> List<T> getAppointments(Periode periode, List<Kamer> kamers, Class<T> type);

	List<RoosterItem> getOneindigeItems();

	List<Object> getRoosterTijden(List<Range<Date>> intervals, RoosterItem roosteritem, Range<Date> totaalInterval);

	List<RoosterItemListViewWrapper> getAlleRoosterBlokkenInPeriode(String sortProperty, boolean asc, RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie);

	List<RoosterItemListViewWrapper> getRoosterBlokken(String sortProperty, boolean asc, long first, long count, RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie);

	long getRoosterBlokkenCount(RoosterListViewFilter filter, ColoscopieCentrum intakeLocatie);

	List<Object> getCurrentRoosterBlokken(Kamer kamer, Range<Date> periode);

	List<ColonBlokkade> getBlokkades(Kamer kamer, Date startTime, Date endTime);

	List<Date> getMdlDatums(Client client, IntakelocatieVanTotEnMetFilter intakeVanTotEnMetFilter);

	List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(String sortProperty, boolean asc, long first, long count, VrijSlotZonderKamerFilter filter);

	List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(String sortProperty, boolean asc, VrijSlotZonderKamerFilter filter);

	List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(VrijSlotZonderKamerFilter filter);

	long getVrijeSlotenZonderKamerCount(VrijSlotZonderKamerFilter filter);

	List<Kamer> getKamers(Date startTijd, Long intakeLocatieId);

	List<ColonBlokkade> getBlokkades(String sortProperty, boolean ascending, long first, long count, RoosterListViewFilter filter, ColoscopieCentrum intakelocatie);

	long getBlokkadesCount(RoosterListViewFilter filter, ColoscopieCentrum intakelocatie);
}
