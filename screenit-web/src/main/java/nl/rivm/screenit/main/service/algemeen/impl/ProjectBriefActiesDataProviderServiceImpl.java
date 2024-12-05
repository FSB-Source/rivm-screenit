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
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectBriefActie_;
import nl.rivm.screenit.repository.algemeen.ProjectBriefActieRepository;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification.filterActief;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification.filterProject;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefActieSpecification.isNietType;

@Service
public class ProjectBriefActiesDataProviderServiceImpl extends RepositoryDataProviderService<ProjectBriefActie, ProjectBriefActieRepository, ProjectBriefActie>
{
	@Override
	protected Specification<ProjectBriefActie> getSpecification(ProjectBriefActie filter, Sort sortParam)
	{
		return isNietType(ProjectBriefActieType.HERINNERING).and(filterProject(filter.getProject())).and(filterActief(filter.getActief()));
	}

	@Override
	protected Order addJoinsForSortingOrCreateDedicatedOrders(Sort.Order order, Root<ProjectBriefActie> r, CriteriaBuilder cb)
	{
		if (order.getProperty().startsWith(ProjectBriefActie_.VRAGENLIJST))
		{
			join(r, ProjectBriefActie_.vragenlijst);
		}
		else if (order.getProperty().startsWith(ProjectBriefActie_.DOCUMENT))
		{
			join(r, ProjectBriefActie_.document);
		}
		return super.addJoinsForSortingOrCreateDedicatedOrders(order, r, cb);
	}
}
