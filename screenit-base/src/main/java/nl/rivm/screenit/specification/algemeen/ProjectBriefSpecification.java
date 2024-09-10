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
import java.util.Date;

import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.MergedBrieven_;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBriefActie_;
import nl.rivm.screenit.model.project.ProjectBrief_;
import nl.rivm.screenit.model.project.ProjectClient_;
import nl.rivm.screenit.model.project.ProjectGroep_;
import nl.rivm.screenit.model.project.ProjectVragenlijstAntwoordenHolder_;
import nl.rivm.screenit.model.project.ProjectVragenlijstStatus;
import nl.rivm.screenit.model.project.Project_;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ProjectBriefSpecification
{

	public static Specification<ProjectBrief> heeftDefinitieVragenlijst()
	{
		return (r, q, cb) ->
		{
			var definitieJoin = SpecificationUtil.join(r, ProjectBrief_.definitie);
			return cb.isNotNull(definitieJoin.get(ProjectBriefActie_.vragenlijst));
		};
	}

	public static Specification<ProjectBrief> heeftPrintDatumNaOfOpDatum(Date verstuurdOp)
	{
		return (r, q, cb) ->
		{
			var mergedBrievenJoin = SpecificationUtil.join(r, ProjectBrief_.mergedBrieven);
			return cb.greaterThanOrEqualTo(mergedBrievenJoin.get(MergedBrieven_.printDatum), verstuurdOp);
		};
	}

	public static Specification<ProjectBrief> heeftDefinitieGelijkAanBaseActie(ProjectBriefActie actie)
	{
		return (r, q, cb) -> cb.equal(r.get(ProjectBrief_.definitie), actie.getBaseActie());
	}

	public static Specification<ProjectBrief> heeftVragenlijstAntwoordenStatusNullOfNietAfgerond()
	{
		return (r, q, cb) ->
		{
			var vragenlijstAntwoordenHolderJoin = SpecificationUtil.join(r, ProjectBrief_.vragenlijstAntwoordenHolder, JoinType.LEFT);
			return cb.or(
				cb.isNull(vragenlijstAntwoordenHolderJoin.get(ProjectVragenlijstAntwoordenHolder_.status)),
				cb.notEqual(vragenlijstAntwoordenHolderJoin.get(ProjectVragenlijstAntwoordenHolder_.status), ProjectVragenlijstStatus.AFGEROND)
			);
		};
	}

	public static Specification<ProjectBrief> heeftNietNullMergedBrievenPrintDatum()
	{
		return (r, q, cb) ->
		{
			var mergedBrievenJoin = SpecificationUtil.join(r, ProjectBrief_.mergedBrieven);
			return cb.isNotNull(mergedBrievenJoin.get(MergedBrieven_.printDatum));
		};
	}

	public static Specification<ProjectBrief> heeftDefinitie(ProjectBriefActie actie)
	{
		return (r, q, cb) -> cb.equal(r.get(ProjectBrief_.definitie), actie);
	}

	public static Specification<ProjectBrief> heeftGeenVerstuurdeBrief(ProjectBriefActie actie)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Client.class);
			var subRoot = subquery.from(ProjectBrief.class);

			subquery.select(subRoot.get(ClientBrief_.client));
			subquery.where(cb.and(
				heeftDefinitie(actie).toPredicate(subRoot, q, cb),
				heeftNietNullMergedBrievenPrintDatum().toPredicate(subRoot, q, cb)
				));
			return cb.not(r.get(ClientBrief_.client).in(subquery));
		};
	}

	public static Specification<ProjectBrief> heeftActieveClientInProjectVoorProjectBrief(LocalDate peildatum)
	{
		return (r, q, cb) ->
		{

			var projectClientJoin = SpecificationUtil.join(r, ProjectBrief_.projectClient);
			var projectGroepJoin = SpecificationUtil.join(projectClientJoin, ProjectClient_.groep);
			var projectJoin = SpecificationUtil.join(projectGroepJoin, ProjectGroep_.project);

			var clientIsActief = cb.isTrue(projectClientJoin.get(ProjectClient_.actief));
			var projectGroepIsActief = cb.isTrue(projectGroepJoin.get(ProjectGroep_.actief));
			var projectIsActief = cb.and(
				cb.lessThanOrEqualTo(projectJoin.get(Project_.startDatum), DateUtil.toUtilDate(peildatum)),
				cb.greaterThan(projectJoin.get(Project_.eindDatum), DateUtil.toUtilDate(peildatum))
			);
			return cb.and(clientIsActief, projectGroepIsActief, projectIsActief);
		};
	}
}
