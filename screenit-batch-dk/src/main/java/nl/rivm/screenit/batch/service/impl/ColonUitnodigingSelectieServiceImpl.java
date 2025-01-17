package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep.ColonClientSelectieContext;
import nl.rivm.screenit.batch.service.ColonUitnodigingSelectieService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification;

import org.apache.commons.lang3.NotImplementedException;
import org.hibernate.ScrollableResults;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification.getSpecificationU1;
import static nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification.getSpecificationU2;

@Service
@AllArgsConstructor
public class ColonUitnodigingSelectieServiceImpl implements ColonUitnodigingSelectieService
{
	private final ClientRepository clientRepository;

	@Override
	public ScrollableResults getUitnodigingsCursor(ColonClientSelectieContext selectieContext, ColonUitnodigingCategorie colonUitnodigingCategorie,
		UitnodigingsGebied uitnodigingsgebied, List<Integer> geboorteJaren, Long projectGroupId, List<Long> exclusieGroepIds)
	{
		return switch (colonUitnodigingCategorie)
		{
			case U1 ->
			{
				var specificationU1 = getSpecificationU1(selectieContext.minimaleLeeftijd, selectieContext.maximaleLeeftijd, selectieContext.peildatum,
					uitnodigingsgebied, geboorteJaren, projectGroupId, exclusieGroepIds, selectieContext.peildatum);
				yield retrieveResults(specificationU1, selectieContext.fetchSize, ColonUitnodigingBaseSpecification::getU1OrderByList);
			}
			case U2 ->
			{
				var specificationU2 = getSpecificationU2(selectieContext.minimaleLeeftijd, selectieContext.maximaleLeeftijd, selectieContext.peildatum, uitnodigingsgebied, null,
					new ArrayList<>(), selectieContext.peildatum);
				yield retrieveResults(specificationU2, selectieContext.fetchSize, ColonUitnodigingBaseSpecification::getU2OrderByList);
			}
			default -> throw new NotImplementedException("Onbekende categorie");
		};
	}

	private ScrollableResults retrieveResults(ExtendedSpecification<Client> specification, int fetchSize, BiFunction<Root<Client>, CriteriaBuilder, List<Order>> orderByList)
	{
		return clientRepository.findWith(specification, q -> q)
			.fetch(g ->
			{
				g.addSubgraph(Client_.persoon).addSubgraph(GbaPersoon_.gbaAdres);
				g.addSubgraph(Client_.colonDossier);
			})
			.setScrollFetchSize(fetchSize)
			.sortBy(orderByList)
			.scroll(-1);
	}
}
