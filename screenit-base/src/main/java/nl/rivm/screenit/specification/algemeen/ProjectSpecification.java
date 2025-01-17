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
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.ProjectParameter_;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBriefActie_;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.model.project.Project_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.util.Pair;

import com.google.common.collect.BoundType;

import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ProjectSpecification
{
	public static ExtendedSpecification<Project> heeftType(ProjectType projectType)
	{
		return (r, q, cb) -> cb.equal(r.get(Project_.type), projectType);
	}

	public static ExtendedSpecification<Project> heeftNaamIn(List<String> projectNamen)
	{
		return (r, q, cb) -> r.get(Project_.naam).in(projectNamen);
	}

	public static ExtendedSpecification<Project> heeftNietNaamIn(List<String> projectNamen)
	{
		return (r, q, cb) -> cb.or(cb.isNull(r.get(Project_.naam)), cb.not(r.get(Project_.naam).in(projectNamen)));
	}

	public static Specification<Project> heeftBriefType(BriefType type)
	{
		return (r, q, cb) ->
		{
			var projectBriefActiesJoin = join(r, Project_.projectBriefActies);
			return cb.equal(projectBriefActiesJoin.get(ProjectBriefActie_.briefType), type);
		};
	}

	public static Specification<Project> heeftEindDatumNaVandaag(LocalDate vandaag)
	{
		return (r, q, cb) ->
			cb.greaterThanOrEqualTo(r.get(Project_.eindDatum), DateUtil.toUtilDate(vandaag));
	}

	public static ExtendedSpecification<Project> isActiefOpDatum(LocalDate peilDatum)
	{

		return bevat(r -> r.get(Project_.startDatum), r -> r.get(Project_.eindDatum), Pair.of(BoundType.CLOSED, BoundType.OPEN),
			DateUtil.toUtilDate(peilDatum));
	}

	public static ExtendedSpecification<Project> heeftParameterKeyMetValue(ProjectParameterKey key)
	{
		return (r, q, cb) ->
		{
			var parameters = join(r, Project_.parameters);
			return cb.and(
				cb.equal(parameters.get(ProjectParameter_.key), key),
				cb.isNotNull(parameters.get(ProjectParameter_.value))
			);
		};
	}

	public static ExtendedSpecification<Project> heeftNaam(String naam)
	{
		return (r, q, cb) -> cb.equal(r.get(Project_.naam), naam);
	}

	public static ExtendedSpecification<Project> heeftStartdatumVanaf(Date date)
	{
		return (r, q, cb) ->
			cb.lessThanOrEqualTo(r.get(Project_.startDatum), date);
	}

}
