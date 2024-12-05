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
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;
import javax.persistence.metamodel.SingularAttribute;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.Dossier_;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.rivm.screenit.model.project.ProjectBriefActie;
import nl.rivm.screenit.model.project.ProjectBrief_;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClient_;
import nl.rivm.screenit.model.project.ProjectGroep_;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftParameterKeyMetValue;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.heeftType;
import static nl.rivm.screenit.specification.algemeen.ProjectSpecification.isActiefOpDatum;
import static nl.rivm.screenit.specification.colon.ColonVolgendeUitnodigingSpecification.heeftProjectPeildatum;

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
			var clientJoin = join(r, ProjectClient_.client);
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
		var dossierJoin = join(clientJoin, dossier, JoinType.LEFT);
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
		return ClientSpecification.heeftActieveClient().with(ProjectClient_.client);
	}

	public static ExtendedSpecification<ProjectClient> isProjectClientActief(boolean isActief)
	{
		return (r, q, cb) -> cb.equal(r.get(ProjectClient_.actief), isActief);
	}

	public static ExtendedSpecification<ProjectClient> heeftActieveProjectGroep()
	{
		return (r, q, cb) ->
		{
			var projectGroepJoin = join(r, ProjectClient_.groep);
			return cb.isTrue(projectGroepJoin.get(ProjectGroep_.actief));
		};
	}

	public static Specification<ProjectClient> filterClient(Client client)
	{
		return skipWhenNull(client, (r, q, cb) -> cb.equal(r.get(ProjectClient_.client), client));
	}

	public static Specification<ProjectClient> heeftPeildatumInVolgendeUitnodiging()
	{
		return heeftProjectPeildatum().with(r ->
		{
			var client = join(r, ProjectClient_.client);
			var dossier = join(client, Client_.colonDossier);
			return join(dossier, ColonDossier_.volgendeUitnodiging);
		});
	}

	public static Specification<ProjectClient> heeftMeerdereProjectenOfHeeftParameterKey(LocalDate peildatum)
	{
		return (r, q, cb) ->
		{
			var subquery = heeftActieveClientenProjectSubquery(peildatum, r, q, cb);

			return cb.or(
				cb.and(
					heeftPeildatumInVolgendeUitnodiging().toPredicate(r, q, cb),
					cb.or(
						cb.not(cb.exists(subquery)),
						r.get(AbstractHibernateObject_.id).in(subquery)
					)
				),
				cb.and(
					heeftParameterKeyMetValue(ProjectParameterKey.COLON_AFWIJKING_UITNODIGINGSINTERVAL).with(ProjectClient_.project).toPredicate(r, q, cb),
					projectClientEnProjectGroepActiefEnProjectActiefOp(peildatum).toPredicate(r, q, cb)
				)
			);
		};
	}

	public static ExtendedSpecification<ProjectClient> projectClientEnProjectGroepActiefEnProjectActiefOp(LocalDate peildatum)
	{
		return isProjectClientActief(true)
			.and(heeftActieveProjectGroep())
			.and(isActiefOpDatum(peildatum).with(ProjectClient_.project));
	}

	private static Subquery<Long> heeftActieveClientenProjectSubquery(LocalDate peildatum, Root<ProjectClient> r, CriteriaQuery<?> q, CriteriaBuilder cb)
	{
		var subquery = q.subquery(Long.class);
		var subRoot = subquery.from(ProjectClient.class);
		var clientJoin = join(subRoot, ProjectClient_.client);

		subquery.select(subRoot.get(AbstractHibernateObject_.id))
			.where(cb.and(
				heeftType(ProjectType.PROJECT).with(ProjectClient_.project).toPredicate(subRoot, q, cb),
				cb.equal(clientJoin.get(SingleTableHibernateObject_.id), r.get(ProjectClient_.client)),
				isProjectClientActief(true).toPredicate(subRoot, q, cb),
				heeftActieveProjectGroep().toPredicate(subRoot, q, cb),
				isActiefOpDatum(peildatum).with(ProjectClient_.project).toPredicate(subRoot, q, cb)
			));
		return subquery;
	}

	public static ExtendedSpecification<ProjectClient> heeftActieveProjectClient(Long projectGroupId)
	{
		return (r, q, cb) -> cb.and(
			cb.equal(r.get(ProjectClient_.groep), projectGroupId),
			cb.isTrue(r.get(ProjectClient_.actief)));
	}

	public static ExtendedSpecification<ProjectClient> clientIsToegevoegdVoorDatum(LocalDate datum)
	{
		return (r, q, cb) -> cb.lessThan(r.get(ProjectClient_.toegevoegd), DateUtil.toUtilDate(datum));
	}

	public static ExtendedSpecification<ProjectClient> heeftIsUitgenodigdInProjectPeriode(boolean value)
	{
		return (r, q, cb) -> cb.equal(r.get(ProjectClient_.isUitgenodigdInProjectPeriode), value);
	}
}
