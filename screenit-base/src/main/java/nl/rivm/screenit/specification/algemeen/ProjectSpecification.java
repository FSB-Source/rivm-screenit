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
import java.util.List;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBriefActie_;
import nl.rivm.screenit.model.project.ProjectClient_;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.model.project.Project_;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ProjectSpecification
{
	public static Specification<Project> heeftProjectType(ProjectType projectType)
	{
		return (r, q, cb) -> cb.equal(r.get(Project_.type), projectType);
	}

	public static Specification<Client> heeftProjectClientVoorDatum(LocalDate datum)
	{
		return (r, q, cb) ->
		{
			var projectClientJoin = SpecificationUtil.join(r, Client_.projecten);
			return cb.lessThan(projectClientJoin.get(ProjectClient_.toegevoegd), DateUtil.toUtilDate(datum));
		};
	}

	public static Specification<Client> heeftClientInProjectMetNaam(List<String> projectNamen)
	{
		return (r, q, cb) ->
		{
			var projectClientJoin = SpecificationUtil.join(r, Client_.projecten);
			var project = SpecificationUtil.join(projectClientJoin, ProjectClient_.project);
			return project.get(Project_.naam).in(projectNamen);
		};
	}

	public static Specification<Project> heeftBriefTypeInProject(BriefType type)
	{
		return (r, q, cb) ->
		{
			var projectBriefActiesJoin = SpecificationUtil.join(r, Project_.projectBriefActies);
			return cb.equal(projectBriefActiesJoin.get(ProjectBriefActie_.briefType), type);
		};
	}

	public static Specification<Project> heeftEindDatumNaVandaag(LocalDate vandaag)
	{
		return (r, q, cb) ->
			cb.greaterThanOrEqualTo(r.get(Project_.eindDatum), DateUtil.toUtilDate(vandaag));
	}

}
