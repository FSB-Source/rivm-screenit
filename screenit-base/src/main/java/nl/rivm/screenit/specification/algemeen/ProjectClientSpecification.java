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

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.metamodel.SingularAttribute;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.Dossier_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBrief_;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClient_;
import nl.rivm.screenit.model.project.ProjectGroep_;
import nl.rivm.screenit.model.project.Project_;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ProjectClientSpecification
{
	public static Specification<ProjectClient> heeftProject(Project project)
	{
		return (r, q, cb) -> cb.equal(r.get(ProjectClient_.project), project);
	}

	public static Specification<ProjectClient> heeftExcludeerAfmelding(List<Bevolkingsonderzoek> onderzoeken)
	{
		return (r, q, cb) ->
		{
			var clientJoin = SpecificationUtil.join(r, ProjectClient_.client);
			Predicate result = null;
			if (onderzoeken.contains(Bevolkingsonderzoek.MAMMA))
			{
				result = heeftGeenAfmeldingVoorBevolkingsonderzoek(cb, clientJoin, Client_.mammaDossier);
			}
			if (onderzoeken.contains(Bevolkingsonderzoek.COLON))
			{
				result = heeftGeenAfmeldingVoorBevolkingsonderzoek(cb, clientJoin, Client_.colonDossier);
			}
			if (onderzoeken.contains(Bevolkingsonderzoek.CERVIX))
			{
				result = heeftGeenAfmeldingVoorBevolkingsonderzoek(cb, clientJoin, Client_.cervixDossier);
			}
			return result;
		};
	}

	private static Predicate heeftGeenAfmeldingVoorBevolkingsonderzoek(CriteriaBuilder cb, Join<ProjectClient, Client> clientJoin,
		SingularAttribute<Client, ? extends Dossier<?, ?>> dossier)
	{
		var dossierJoin = SpecificationUtil.join(clientJoin, dossier, JoinType.LEFT);
		return cb.or(dossierJoin.get(TablePerClassHibernateObject_.id).isNull(), cb.equal(dossierJoin.get(Dossier_.aangemeld), true));
	}

	public static Specification<ProjectClient> isNietInProjectBrief(ProjectBriefActie definitie)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var projectBriefRoot = subquery.from(ProjectBrief.class);
			subquery.where(cb.equal(projectBriefRoot.get(ProjectBrief_.definitie), definitie));
			subquery.select(projectBriefRoot.get(ProjectBrief_.projectClient).get(AbstractHibernateObject_.id)).distinct(true);

			return cb.not(r.get(AbstractHibernateObject_.id).in(subquery));
		};
	}

	public static Specification<ProjectClient> heeftActieveClient()
	{
		return ClientSpecification.heeftActieveClientPredicate()
			.toSpecification(r ->
				join(r, ProjectClient_.client));
	}

	public static Specification<ProjectClient> heeftActieveClientInProjectVoorProjectClient(LocalDate peildatum)
	{
		return (r, q, cb) ->
		{
			var projectGroepJoin = SpecificationUtil.join(r, ProjectClient_.groep);
			var projectJoin = SpecificationUtil.join(projectGroepJoin, ProjectGroep_.project);

			var clientIsActief = cb.isTrue(r.get(ProjectClient_.actief));
			var projectgroepIsActief = cb.isTrue(projectGroepJoin.get(ProjectGroep_.actief));
			var projectIsActief = cb.and(
				cb.lessThanOrEqualTo(projectJoin.get(Project_.startDatum), DateUtil.toUtilDate(peildatum)),
				cb.greaterThan(projectJoin.get(Project_.eindDatum), DateUtil.toUtilDate(peildatum))
			);
			return cb.and(clientIsActief, projectgroepIsActief, projectIsActief);
		};
	}
}
