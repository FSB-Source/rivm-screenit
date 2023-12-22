package nl.rivm.screenit.service.cervix.impl;

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

import java.util.Optional;

import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.repository.cervix.CervixHuisartsRepository;
import nl.rivm.screenit.service.cervix.CervixHuisartsLocatieService;
import nl.rivm.screenit.specification.cervix.CervixHuisartsSpecification;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class CervixHuisartsLocatieServiceImpl implements CervixHuisartsLocatieService
{
	@Autowired
	private CervixHuisartsRepository huisartsRepository;

	@Override
	public Optional<CervixHuisarts> findActieveHuisartsMetEenActieveLocatie(String agbcode)
	{
		var specification = CervixHuisartsSpecification.heeftAgbCode(agbcode)
			.and(CervixHuisartsSpecification.isActief())
			.and(CervixHuisartsSpecification.heeftActieveLocatie());

		return huisartsRepository.findOne(specification);
	}
}
