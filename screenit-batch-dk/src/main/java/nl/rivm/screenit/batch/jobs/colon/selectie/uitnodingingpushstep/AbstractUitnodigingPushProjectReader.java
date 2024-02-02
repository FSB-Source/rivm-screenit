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

import nl.rivm.screenit.batch.jobs.colon.selectie.AbstractUitnodigingPushReader;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingCategorie;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.query.ScreenitRestrictions;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;

abstract class AbstractUitnodigingPushProjectReader extends AbstractUitnodigingPushReader
{

	protected AbstractUitnodigingPushProjectReader(ColonUitnodigingCategorie categorie)
	{
		super(categorie);
	}

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var vandaag = currentDateSupplier.getLocalDate();

		var crit = session.createCriteria(ProjectClient.class);
		crit.createAlias("project", "project", JoinType.INNER_JOIN);
		crit.createAlias("groep", "groep", JoinType.INNER_JOIN);
		crit.createAlias("client", "client", JoinType.INNER_JOIN);
		createBaseCriteria(crit);

		crit.add(Restrictions.eq("isUitgenodigdInProjectPeriode", Boolean.FALSE));

		crit.add(ScreenitRestrictions.addClientActiefInProjectCriteria("", "groep", "project", vandaag));

		crit.add(Restrictions.eq("groep.uitnodigingenPushenNa", DateUtil.toUtilDate(vandaag)));

		return crit;
	}
}
