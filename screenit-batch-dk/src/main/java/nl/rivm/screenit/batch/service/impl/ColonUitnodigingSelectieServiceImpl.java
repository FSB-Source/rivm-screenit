package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.List;
import java.util.function.Function;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.colon.selectie.selectiestep.ColonClientSelectieContext;
import nl.rivm.screenit.batch.service.ColonUitnodigingSelectieService;
import nl.rivm.screenit.dao.colon.ColonUitnodigingsDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.apache.commons.lang3.NotImplementedException;
import org.hibernate.ScrollableResults;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftGeenActieveProjectClienten;
import static nl.rivm.screenit.specification.algemeen.DossierSpecification.wachtOpStartProject;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.heeftActieveProjectClient;
import static nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification.clientUitnodigingBase;
import static nl.rivm.screenit.specification.colon.ColonUitnodigingBaseSpecification.u1Base;
import static org.apache.commons.collections4.CollectionUtils.isNotEmpty;

@Service
@AllArgsConstructor
public class ColonUitnodigingSelectieServiceImpl implements ColonUitnodigingSelectieService
{
	private final ClientRepository clientRepository;

	private final ColonUitnodigingsDao uitnodigingsDao;

	@Override
	public ScrollableResults getUitnodigingsCursor(ColonClientSelectieContext selectieContext, ColonUitnodigingCategorie colonUitnodigingCategorie,
		UitnodigingsGebied uitnodigingsgebied, List<Integer> geboorteJaren, Long projectGroupId, List<Long> exclusieGroepIds)
	{
		switch (colonUitnodigingCategorie)
		{
		case U1:
			var specification =
				clientUitnodigingBase(selectieContext.minimaleLeeftijd, selectieContext.maximaleLeeftijd, selectieContext.peildatum, uitnodigingsgebied)
					.and(u1Base(selectieContext.peildatum, selectieContext.peildatum, geboorteJaren))
					.and(wachtOpStartProject(Boolean.FALSE).with(r -> dossierJoin(r)));

			specification = addProjectClientSpecifications(projectGroupId, exclusieGroepIds, specification);

			return clientRepository.findWith(specification, q -> q)
				.fetch(g ->
				{
					g.addSubgraph(Client_.persoon).addSubgraph(GbaPersoon_.gbaAdres);
					g.addSubgraph(Client_.colonDossier);
				})
				.setScrollFetchSize(selectieContext.fetchSize)
				.sortBy((r, cb) -> List.of(cb.asc(persoonJoin(r).get(GbaPersoon_.geboortedatum)), cb.asc(persoonJoin(r).get(GbaPersoon_.achternaam))))
				.scroll(-1);

		case U2:
			return uitnodigingsDao.getUitnodigingsCursorU2(uitnodigingsgebied, selectieContext.minimaleLeeftijd, selectieContext.maximaleLeeftijd, projectGroupId,
				exclusieGroepIds, selectieContext.fetchSize);
		default:
			throw new NotImplementedException("Onbekende categorie");
		}
	}

	private ExtendedSpecification<Client> addProjectClientSpecifications(Long projectGroupId, List<Long> exclusieGroepIds, ExtendedSpecification<Client> specification)
	{
		if (projectGroupId != null)
		{
			specification = specification.and(heeftActieveProjectClient(projectGroupId).with(projectClientJoin()));
		}
		else if (isNotEmpty(exclusieGroepIds))
		{
			specification = specification.and(heeftGeenActieveProjectClienten(exclusieGroepIds));
		}
		return specification;
	}

	private Join<?, ? extends GbaPersoon> persoonJoin(From<?, ? extends Client> r)
	{
		return join(r, Client_.persoon);
	}

	private Join<?, ? extends ColonDossier> dossierJoin(From<?, ? extends Client> r)
	{
		return join(r, Client_.colonDossier, JoinType.LEFT);
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends ProjectClient>> projectClientJoin()
	{
		return q -> join(q, Client_.projecten);
	}
}
