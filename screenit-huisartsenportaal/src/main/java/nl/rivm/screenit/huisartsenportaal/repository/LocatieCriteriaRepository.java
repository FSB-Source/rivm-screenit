package nl.rivm.screenit.huisartsenportaal.repository;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.EnumSet;
import java.util.List;

import nl.rivm.screenit.huisartsenportaal.dto.locatie.LocatieSearchDto;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;

public interface LocatieCriteriaRepository
{
	List<Locatie> getLocaties(Huisarts huisarts, LocatieSearchDto locatieSearchDto);

	List<Locatie> findByHuisartsAndStatussen(Huisarts huisarts, EnumSet<CervixLocatieStatus> statussen);

	long countLocaties(Huisarts huisarts, LocatieSearchDto locatieSearchDto);
}
