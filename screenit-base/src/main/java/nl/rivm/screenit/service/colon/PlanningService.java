package nl.rivm.screenit.service.colon;

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

import java.time.LocalDate;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.planning.VrijSlot;
import nl.rivm.screenit.model.colon.planning.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.planning.VrijSlotZonderKamerFilter;

public interface PlanningService<T extends VrijSlot>
{

	List<T> getBeschikbaarheid(LocalDate startTime, LocalDate endTime, ColoscopieCentrum intakelocatie);

	List<VrijSlotZonderKamer> getVrijeSlotenZonderKamer(String sortProperty, boolean asc, long first, long count, VrijSlotZonderKamerFilter filter, Client client);

	long getVrijeSlotenZonderKamerCount(VrijSlotZonderKamerFilter filter, Client client);

	Kamer getBeschikbareKamer(Date startTijd, Long intakeLocatieId);
}
