package nl.rivm.screenit.main.service.algemeen.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.service.RepositoryDataProviderService;
import nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry;
import nl.rivm.screenit.repository.algemeen.ProjectBestandVerwerkingEntryRepository;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.algemeen.ProjectBestandVerwerkingEntrySpecification.heeftVerwerking;

@Service
public class ProjectBestandVerwerkingDataProviderServiceImpl
	extends RepositoryDataProviderService<ProjectBestandVerwerkingEntry, ProjectBestandVerwerkingEntryRepository, ProjectBestandVerwerkingEntry>
{
	@Override
	protected Specification<ProjectBestandVerwerkingEntry> getSpecification(ProjectBestandVerwerkingEntry filter, Sort sort)
	{
		return heeftVerwerking(filter.getVerwerking());
	}
}
