package nl.rivm.screenit.batch.jobs.cervix.cismigranten;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class CervixCISMigrantenReader extends BaseScrollableResultReader
{
	private final ICurrentDateSupplier dateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession statelessSession) throws HibernateException
	{
		var criteria = statelessSession.createCriteria(Client.class, "client");
		criteria.createAlias("client.projecten", "projectclient");
		criteria.add(Restrictions.eq("projectclient.actief", true));
		criteria.createAlias("projectclient.groep", "projectgroep");
		criteria.add(Restrictions.eq("projectgroep.actief", true));

		criteria.createAlias("projectclient.project", "project");
		criteria.add(Restrictions.le("project.startDatum", dateSupplier.getDate()));
		criteria.add(Restrictions.eq("project.naam", "CIS-Migranten"));

		criteria.createAlias("client.persoon", "persoon");

		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");

		criteria.createAlias("client.cervixDossier", "dossier", JoinType.LEFT_OUTER_JOIN);

		criteria.createAlias("dossier.cisHistorie", "cisHistorie", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("dossier.laatsteScreeningRonde", "laatsteScreeningRonde", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("cisHistorie.screeningRonde", "screeningRonde", JoinType.LEFT_OUTER_JOIN);

		criteria.add(Restrictions.isNull("screeningRonde.uitstel"));

		criteria.add(Restrictions.or(Restrictions.isNull("cisHistorie.screeningRonde"), Restrictions.eqProperty("cisHistorie.screeningRonde", "dossier.laatsteScreeningRonde")));

		criteria.add(Restrictions.isNull("laatsteScreeningRonde.laatsteUitnodiging"));

		return criteria;
	}
}
