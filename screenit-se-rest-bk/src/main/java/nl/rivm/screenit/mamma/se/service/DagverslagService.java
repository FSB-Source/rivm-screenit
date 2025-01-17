package nl.rivm.screenit.mamma.se.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Map;

import nl.rivm.screenit.mamma.se.dto.DagAfsluitingDto;
import nl.rivm.screenit.mamma.se.dto.DagPlanningSamenvattingDto;
import nl.rivm.screenit.mamma.se.dto.DagProductieDto;
import nl.rivm.screenit.mamma.se.dto.DagSynchronisatieDto;

public interface DagverslagService
{
	Map<String, DagProductieDto> getDagproductieVanSeMedewerkers(String seCode, Date datum);

	DagAfsluitingDto getDoorgevoerdCountVanDag(String seCode, LocalDate datum);

	DagSynchronisatieDto getSynchronisatieCountVanDag(String seCode, LocalDate datum);

	LocalDate getDatumVanOudsteNietAfgeslotenOnderzoek(String seCode);

	DagPlanningSamenvattingDto getPlanningSamenvattingVanDeDag(String seCode, Date datum);
}
