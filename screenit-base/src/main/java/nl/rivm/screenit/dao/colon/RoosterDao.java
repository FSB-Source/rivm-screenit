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

import java.time.LocalDateTime;
import java.util.List;

import nl.rivm.screenit.model.colon.ColonAfspraakslotListViewWrapper;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamerFilter;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot;

import com.google.common.collect.Range;

public interface RoosterDao
{

	<T extends ColonTijdslot> List<T> zoekTijdslotsVoorKamersInRange(Range<LocalDateTime> range, List<ColonIntakekamer> kamers, Class<T> type);

	List<ColonAfspraakslotListViewWrapper> getAlleAfspraakslotsInPeriode(String sortProperty, boolean asc, RoosterListViewFilter filter, ColonIntakelocatie intakeLocatie);

	List<ColonAfspraakslotListViewWrapper> getAfspraakslots(String sortProperty, boolean asc, long first, long count, RoosterListViewFilter filter,
		ColonIntakelocatie intakeLocatie);

	long getAfspraakslotsCount(RoosterListViewFilter filter, ColonIntakelocatie intakeLocatie);

	List<Object> getCurrentAfspraakslots(ColonIntakekamer kamer, Range<LocalDateTime> periode);

	List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(String sortProperty, boolean asc, long first, long count, VrijSlotZonderKamerFilter filter);

	List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(String sortProperty, boolean asc, VrijSlotZonderKamerFilter filter);

	List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(VrijSlotZonderKamerFilter filter);

	long getVrijeSlotenZonderKamerCount(VrijSlotZonderKamerFilter filter);

	List<ColonIntakekamer> getKamers(LocalDateTime startTijd, Long intakelocatieId);

	List<ColonBlokkade> getBlokkades(String sortProperty, boolean ascending, long first, long count, RoosterListViewFilter filter, ColonIntakelocatie intakelocatie);

	List<ColonBlokkade> getBlokkades(ColonIntakekamer kamer, LocalDateTime vanaf, LocalDateTime tot);

	long getBlokkadesCount(RoosterListViewFilter filter, ColonIntakelocatie intakelocatie);
}
