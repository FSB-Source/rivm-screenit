package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import javax.persistence.EntityGraph;

import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.EnovationHuisarts_;
import nl.rivm.screenit.repository.algemeen.EnovationHuisartsRepository;
import nl.rivm.screenit.service.EnovationHuisartsService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

@Service
public class EnovationHuisartsDataProviderServiceImpl extends RepositoryDataProviderService<EnovationHuisarts, EnovationHuisartsRepository, EnovationHuisarts>
{
	@Autowired
	private EnovationHuisartsService huisartsService;

	@Override
	protected Specification<EnovationHuisarts> getSpecification(EnovationHuisarts filter, Sort sortParam)
	{
		return huisartsService.getHuisartsenSpecification(filter);
	}

	@Override
	protected void fetch(EntityGraph<EnovationHuisarts> entityGraph)
	{
		entityGraph.addSubgraph(EnovationHuisarts_.ADRES);
		super.fetch(entityGraph);
	}
}
