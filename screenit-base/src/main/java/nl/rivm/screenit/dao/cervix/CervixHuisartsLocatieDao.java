package nl.rivm.screenit.dao.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsLocatieMutatieSoort;
import nl.rivm.screenit.service.cervix.CervixHuisartsLocatieFilter;

public interface CervixHuisartsLocatieDao
{
	List<CervixHuisartsLocatie> getHuisartsLocaties(CervixHuisartsLocatieFilter filter, long first, long count, String sortProperty, boolean asc);

	List<CervixHuisartsLocatie> getHuisartsLocaties(long first, long count, String orderByProperty, boolean ascending,
		String agbCode, List<CervixHuisartsLocatieMutatieSoort> mutatiesoorten, Date mutatiedatumVanaf, Date mutatiedatumTot, List<Gemeente> gemeentes);

	long countHuisartsLocaties(CervixHuisartsLocatieFilter filter);

	long countHuisartsLocaties(String agbCode, List<CervixHuisartsLocatieMutatieSoort> mutatiesoorten, Date mutatiedatumVanaf, Date mutatiedatumTot, List<Gemeente> gemeentes);

	CervixHuisarts getActieveHuisartsMetEenActieveLocatie(String agb);
}
