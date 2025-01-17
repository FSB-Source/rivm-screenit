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

import java.time.LocalDate;
import java.util.Date;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.ProjectParameter_;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.model.project.ProjectGroep_;
import nl.rivm.screenit.model.project.Project_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ProjectGroepSpecification
{

	public static Specification<ProjectGroep> filterProject(Project project)
	{
		return skipWhenNull(project, (r, q, cb) -> cb.equal(r.get(ProjectGroep_.project), project));
	}

	public static Specification<ProjectGroep> filterActief(Boolean actief)
	{
		return skipWhenNull(actief, (r, q, cb) -> cb.equal(r.get(ProjectGroep_.actief), actief));
	}

	public static Specification<ProjectGroep> getActieveProjectGroepenVoorUitnodigingDK(Date peilMoment)
	{
		return (r, q, cb) ->
		{
			var project = join(r, ProjectGroep_.project);
			var parameters = join(project, Project_.parameters);
			return cb.and(
				cb.lessThanOrEqualTo(r.get(ProjectGroep_.actiefDatum), peilMoment),
				cb.greaterThan(r.get(ProjectGroep_.uitnodigenVoorDKvoor), peilMoment),
				cb.isTrue(r.get(ProjectGroep_.actief)),
				cb.greaterThan(r.get(ProjectGroep_.populatie), 0),
				cb.equal(parameters.get(ProjectParameter_.key), ProjectParameterKey.COLON_UITNODIGEN_PRIORITEIT),
				cb.isNotNull(parameters.get(ProjectParameter_.value)));
		};
	}

	public static ExtendedSpecification<ProjectGroep> heeftUitnodigingenPushenNa(LocalDate vandaag)
	{
		return (r, q, cb) -> cb.equal(r.get(ProjectGroep_.uitnodigingenPushenNa), DateUtil.toUtilDate(vandaag));
	}
}
