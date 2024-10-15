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

import java.time.LocalDate;
import java.util.List;

import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot;

public interface AfspraakDao
{

	List<ColonIntakeAfspraak> getAfsprakenVoorIntakelocatie(WerklijstIntakeFilter zoekObject, ColonIntakelocatie intakelocatie, LocalDate vandaag, long first, long count,
		String property, boolean ascending);

	long countAfsprakenVoorIntakelocatie(WerklijstIntakeFilter zoekObject, ColonIntakelocatie intakelocatie, LocalDate vandaag);

	void saveOrUpdate(ColonTijdslot tijdslot);

	void delete(ColonTijdslot tijdslot);
}
