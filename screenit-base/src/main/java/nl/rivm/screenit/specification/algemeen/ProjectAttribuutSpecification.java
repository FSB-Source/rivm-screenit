package nl.rivm.screenit.specification.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectAttribuut_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ProjectAttribuutSpecification
{

	public static Specification<ProjectAttribuut> heeftProject(Project project)
	{
		return (r, q, cb) -> cb.equal(r.get(ProjectAttribuut_.project), project);
	}

	public static Specification<ProjectAttribuut> heeftMergeField(String mergeField)
	{
		return (r, q, cb) -> cb.equal(r.get(ProjectAttribuut_.mergeField), mergeField);
	}

	public static Specification<ProjectAttribuut> heeftNaam(String naam)
	{
		return (r, q, cb) -> cb.equal(r.get(ProjectAttribuut_.naam), naam);
	}

	public static Specification<ProjectAttribuut> filterActief(Boolean actief)
	{
		return skipWhenNull(actief, (r, q, cb) -> cb.equal(r.get(ProjectAttribuut_.actief), actief));
	}
}
