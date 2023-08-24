package nl.rivm.screenit.mamma.planning.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.time.LocalDate;
import java.time.LocalTime;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.mamma.planning.model.PlanningDag;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningWeek;

public interface PlanningCapaciteitAgendaService
{
	void herhalen(PlanningScreeningsEenheid bronScreeningsEenheid, PlanningScreeningsEenheid doelScreeningsEenheid, PlanningWeek teHerhalenWeek, LocalDate herhalenVanaf,
		LocalDate herhalenTotEnMet);

	void herhalen(LocalDate herhalenVanaf);

	PlanningDag kopieerDag(PlanningScreeningsEenheid bronScreeningsEenheid, PlanningScreeningsEenheid doelScreeningsEenheid, LocalDate bronDate, LocalTime bronVanTijd,
		LocalTime bronTotTijd, LocalDate doelDate) throws OpslaanVerwijderenTijdBlokException;
}
