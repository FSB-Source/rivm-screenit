package nl.rivm.screenit.batch.jobs.colon.selectie.projectinterval;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.ProjectParameterKey;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.model.project.ProjectType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class ColonProjectIntervalToepassenReader extends BaseScrollableResultReader
{

	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var vandaag = currentDateSupplier.getLocalDate();

		var criteria = session.createCriteria(ProjectClient.class, "projectClient");
		criteria.createAlias("projectClient.client", "client");
		criteria.createAlias("client.colonDossier", "dossier");
		criteria.createAlias("dossier.volgendeUitnodiging", "volgendeUitnodiging");
		criteria.createAlias("projectClient.groep", "groep");
		criteria.createAlias("projectClient.project", "project");
		criteria.createAlias("project.parameters", "parameter");

		DetachedCriteria activeProjectClientQuery = DetachedCriteria.forClass(ProjectClient.class, "sProjectClient");
		activeProjectClientQuery.createAlias("sProjectClient.client", "sClient");
		activeProjectClientQuery.createAlias("sProjectClient.groep", "sGroep");
		activeProjectClientQuery.createAlias("sProjectClient.project", "sProject");

		activeProjectClientQuery.add(Restrictions.eq("sProject.type", ProjectType.PROJECT));
		activeProjectClientQuery.add(ScreenitRestrictions.addClientActiefInProjectCriteria("sProjectClient", "sGroep", "sProject", vandaag));
		activeProjectClientQuery.add(Restrictions.eqProperty("sClient.id", "projectClient.client"));
		activeProjectClientQuery.setProjection(Projections.id());

		criteria.add(Restrictions.eq("project.type", ProjectType.PROJECT));
		criteria.add(
			Restrictions.or(
				Restrictions.and(Restrictions.isNotNull("volgendeUitnodiging.projectPeildatum"),
					Restrictions.or(
						Subqueries.notExists(activeProjectClientQuery),
						Subqueries.propertyEq("projectClient.id", activeProjectClientQuery)
					)
				),
				Restrictions.and(
					Restrictions.eq("parameter.key", ProjectParameterKey.COLON_AFWIJKING_UITNODIGINGSINTERVAL),
					Restrictions.isNotNull("parameter.value"),
					ScreenitRestrictions.addClientActiefInProjectCriteria("projectClient", "groep", "project", vandaag)
				)
			)
		);

		return criteria;
	}
}
