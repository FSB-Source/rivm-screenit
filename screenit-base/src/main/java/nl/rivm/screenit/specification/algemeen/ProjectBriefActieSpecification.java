package nl.rivm.screenit.specification.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActieType;
import nl.rivm.screenit.model.project.ProjectBriefActie_;
import nl.rivm.screenit.model.project.ProjectClient_;
import nl.rivm.screenit.model.project.ProjectGroep_;
import nl.rivm.screenit.model.project.Project_;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ProjectBriefActieSpecification
{
	public static Specification<ProjectBriefActie> heeftBriefTypeInProjectBriefActie(BriefType briefType)
	{
		return (r, q, cb) -> cb.equal(r.get(ProjectBriefActie_.briefType), briefType);
	}

	public static Specification<ProjectBriefActie> isProjectBriefActieTypeVervangendeBrief()
	{
		return (r, q, cb) -> cb.equal(r.get(ProjectBriefActie_.type), ProjectBriefActieType.VERVANGENDEBRIEF);
	}

	public static Specification<ProjectBriefActie> heeftClient(Client client)
	{
		return (r, q, cb) ->
		{
			var projectJoin = SpecificationUtil.join(r, ProjectBriefActie_.project);
			var clientenJoin = SpecificationUtil.join(projectJoin, Project_.clienten);
			return cb.equal(clientenJoin.get(ProjectClient_.client), client);
		};
	}

	public static Specification<ProjectBriefActie> heeftActieveClientInProjectVoorProjectBriefActie(LocalDate peildatum)
	{
		return (r, q, cb) ->
		{
			var projectJoin = SpecificationUtil.join(r, ProjectBriefActie_.project);
			var projectClientJoin = SpecificationUtil.join(projectJoin, Project_.clienten);
			var projectGroepJoin = SpecificationUtil.join(projectClientJoin, ProjectClient_.groep);

			var clientIsActief = cb.isTrue(projectClientJoin.get(ProjectClient_.actief));
			var projectgroepIsActief = cb.isTrue(projectGroepJoin.get(ProjectGroep_.actief));
			var projectIsActief = cb.and(
				cb.lessThanOrEqualTo(projectJoin.get(Project_.startDatum), DateUtil.toUtilDate(peildatum)),
				cb.greaterThan(projectJoin.get(Project_.eindDatum), DateUtil.toUtilDate(peildatum))
			);
			return cb.and(clientIsActief, projectgroepIsActief, projectIsActief);
		};
	}
}
