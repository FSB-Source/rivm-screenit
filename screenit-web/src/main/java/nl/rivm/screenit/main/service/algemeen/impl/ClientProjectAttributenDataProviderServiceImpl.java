package nl.rivm.screenit.main.service.algemeen.impl;

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

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClientAttribuut;
import nl.rivm.screenit.model.project.ProjectClientAttribuut_;
import nl.rivm.screenit.repository.algemeen.ProjectClientAttribuutRepository;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ProjectClientAttribuutSpecification.heeftProjectClient;

@Service("clientProjectAttributenDataProviderServiceImpl")
public class ClientProjectAttributenDataProviderServiceImpl
	extends RepositoryDataProviderService<ProjectClientAttribuut, ProjectClientAttribuutRepository, ProjectClient>
{
	@Override
	protected Specification<ProjectClientAttribuut> getSpecification(ProjectClient filter, Sort sort)
	{
		return heeftProjectClient(filter);
	}

	@Override
	protected Order addJoinsForSortingOrCreateDedicatedOrders(Sort.Order order, Root<ProjectClientAttribuut> r, CriteriaBuilder cb)
	{
		join(r, ProjectClientAttribuut_.attribuut);
		return super.addJoinsForSortingOrCreateDedicatedOrders(order, r, cb);
	}
}
