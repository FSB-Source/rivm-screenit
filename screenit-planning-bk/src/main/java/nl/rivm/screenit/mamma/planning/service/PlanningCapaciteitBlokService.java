package nl.rivm.screenit.mamma.planning.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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
import java.util.Set;

import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;

public interface PlanningCapaciteitBlokService
{

	Set<PlanningBlok> getCapaciteitsBlokkenVanDag(PlanningScreeningsEenheid screeningsEenheid, LocalDate bronDate);

	PlanningBlok maakBlok(PlanningCapaciteitBlokDto blokDto) throws OpslaanVerwijderenTijdBlokException;

	void verwijderBlok(PlanningBlok blok) throws OpslaanVerwijderenTijdBlokException;

}
