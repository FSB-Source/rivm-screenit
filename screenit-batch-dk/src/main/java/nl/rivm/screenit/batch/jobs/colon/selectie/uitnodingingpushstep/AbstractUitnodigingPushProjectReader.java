package nl.rivm.screenit.batch.jobs.colon.selectie.uitnodingingpushstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;

import nl.rivm.screenit.batch.jobs.colon.selectie.AbstractUitnodigingPushReader;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Gemeente_;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectClient_;
import nl.rivm.screenit.specification.algemeen.ProjectGroepSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.heeftIsUitgenodigdInProjectPeriode;
import static nl.rivm.screenit.specification.algemeen.ProjectClientSpecification.projectClientEnProjectGroepActiefEnProjectActiefOp;

abstract class AbstractUitnodigingPushProjectReader extends AbstractUitnodigingPushReader<ProjectClient>
{

	protected AbstractUitnodigingPushProjectReader(ColonUitnodigingCategorie categorie)
	{
		super(categorie);
	}

	@Override
	protected Specification<ProjectClient> createSpecification()
	{
		var vandaag = currentDateSupplier.getLocalDate();
		return baseSpecifications().with(ProjectClient_.client)
			.and(heeftIsUitgenodigdInProjectPeriode(false))
			.and(projectClientEnProjectGroepActiefEnProjectActiefOp(vandaag))
			.and(ProjectGroepSpecification.heeftUitnodigingenPushenNa(vandaag).with(ProjectClient_.groep));
	}

	@Override
	protected List<Selection<?>> createProjections(Root<ProjectClient> r, CriteriaBuilder cb)
	{
		return List.of(
			clientJoin(r).get(SingleTableHibernateObject_.id),
			join(r, ProjectClient_.groep).get(AbstractHibernateObject_.id),
			gemeenteJoin(r).get(AbstractHibernateObject_.id),
			join(gemeenteJoin(r), Gemeente_.screeningOrganisatie, JoinType.LEFT).get(SingleTableHibernateObject_.id));
	}

	private static Join<BagAdres, Gemeente> gemeenteJoin(Root<ProjectClient> r)
	{
		return join(join(join(clientJoin(r), Client_.persoon), GbaPersoon_.gbaAdres), BagAdres_.gbaGemeente);
	}

	private static Join<ProjectClient, Client> clientJoin(Root<ProjectClient> r)
	{
		return join(r, ProjectClient_.client);
	}

}
