package nl.rivm.screenit.batch.repository;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import java.util.List;

import javax.persistence.criteria.From;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBrief_;
import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientBriefSpecification.heeftScreeningsOrganisatie;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.algemeen.ProjectBriefSpecification.heeftGeenMergedBrieven;

public interface ProjectBriefRepository extends BaseJpaRepository<ProjectBrief>
{
	default List<Long> getActieveProjectBriefActieDefinities(ScreeningOrganisatie screeningOrganisatie)
	{
		var specification = heeftGeenMergedBrieven()
			.and(heeftScreeningsOrganisatie(screeningOrganisatie))
			.and(heeftActieveClient().with(r -> clientJoin(r)));

		return findWith(specification, Long.class, q -> q.projection((cb, r) -> r.get(ProjectBrief_.definitie).get(AbstractHibernateObject_.id)).distinct().all());
	}

	private From<?, ? extends Client> clientJoin(From<?, ? extends ProjectBrief> projectbriefRoot)
	{
		return join(projectbriefRoot, ClientBrief_.client);
	}
}
