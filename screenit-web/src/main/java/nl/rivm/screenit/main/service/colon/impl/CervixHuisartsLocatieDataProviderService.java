package nl.rivm.screenit.main.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.repository.cervix.CervixHuisartsLocatieRepository;
import nl.rivm.screenit.service.cervix.CervixHuisartsLocatieFilter;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification.filterHuisartsMetAgbCodeContaining;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification.filterOpAchternaamMedewerkerContaining;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification.filterOpLocatieNaamContaining;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification.filterOpPlaatsContaining;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification.filterOpPostcodeContaining;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification.filterOpStraat;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification.heeftGeregistreerdeHuisarts;
import static nl.rivm.screenit.specification.cervix.CervixHuisartsLocatieSpecification.isVolledig;

@Service
public class CervixHuisartsLocatieDataProviderService extends RepositoryDataProviderService<CervixHuisartsLocatie, CervixHuisartsLocatieRepository, CervixHuisartsLocatieFilter>
{
	@Override
	protected Specification<CervixHuisartsLocatie> getSpecification(CervixHuisartsLocatieFilter filter)
	{
		return filterOpAchternaamMedewerkerContaining(filter.getAchternaam())
			.and(filterHuisartsMetAgbCodeContaining(filter.getAgbcode()))
			.and(filterOpLocatieNaamContaining(filter.getLocatieNaam()))
			.and(filterOpPostcodeContaining(filter.getPostcode()))
			.and(filterOpPlaatsContaining(filter.getPlaats()))
			.and(filterOpStraat(filter.getStraat()))
			.and(isVolledig())
			.and(heeftGeregistreerdeHuisarts());
	}
}
