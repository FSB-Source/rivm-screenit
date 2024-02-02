package nl.rivm.screenit.dao.impl;

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

import java.util.Date;
import java.util.List;

import javax.annotation.Nonnull;

import nl.rivm.screenit.dao.DashboardDao;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.dashboard.DashboardLogRegel;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.dashboard.DashboardType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class DashboardDaoImpl extends AbstractAutowiredDao implements DashboardDao
{

	@Override
	public List<DashboardStatus> getDashboardStatussen(DashboardType item)
	{
		var criteria = new BaseCriteria<DashboardStatus>(DashboardStatus.class);
		criteria.add(Restrictions.eq("type", item));
		return criteria.list(getSession());
	}

	@Override
	public List<DashboardLogRegel> getLogRegelsInDashboard(DashboardType dashboardType)
	{
		var criteria = getSession().createCriteria(DashboardLogRegel.class);
		criteria.createAlias("dashboardStatus", "dashboardStatus");
		criteria.add(Restrictions.eq("dashboardStatus.type", dashboardType));
		return criteria.list();
	}

	@Override
	public void maakDashboardStatusLeeg(@Nonnull DashboardType dashboardType)
	{
		if (dashboardType == DashboardType.GBA_LANDELIJK)
		{
			var gbaSql = "delete from gedeeld.dashboard_log_regel where id in (select dl.id from gedeeld.dashboard_log_regel as dl join dashboard_status as ds on dl.dashboard_status = ds.id join log_regel as lr on dl.log_regel = lr.id join log_event as le on lr.log_event = le.id where type = :ty and le.level = :le);";
			var gbaQuery = getSession().createSQLQuery(gbaSql);
			gbaQuery.setString("ty", dashboardType.name());
			gbaQuery.setString("le", Level.INFO.name());
			gbaQuery.executeUpdate();
		}
		else
		{
			var sql = "delete from gedeeld.dashboard_log_regel where dashboard_status in (select id from gedeeld.dashboard_status where type = :di)";
			var query = getSession().createSQLQuery(sql);
			query.setString("di", dashboardType.name());
			query.executeUpdate();
		}
	}

	@Override
	public Date getDateTimeLastLogRegel(@Nonnull DashboardType dashboardType)
	{
		Criteria criteria = getSession().createCriteria(DashboardLogRegel.class);
		criteria.createAlias("logRegel", "logRegel");
		criteria.createAlias("dashboardStatus", "dashboardStatus");
		criteria.add(Restrictions.eq("dashboardStatus.type", dashboardType));
		criteria.setProjection(Projections.max("logRegel.gebeurtenisDatum"));
		return (Date) criteria.uniqueResult();
	}

	@Override
	public List<DashboardStatus> getListOfDashboardStatussen(Instelling ingelogdVoorOrganisatie)
	{
		BaseCriteria<DashboardStatus> criteria = new BaseCriteria<DashboardStatus>(DashboardStatus.class);
		criteria.add(Restrictions.eq("organisatie", ingelogdVoorOrganisatie));
		return criteria.list(getSession());
	}

	@Override
	public List<DashboardLogRegel> getDashboardLogRegelMetLogRegel(LogRegel logRegel)
	{
		BaseCriteria<DashboardLogRegel> criteria = new BaseCriteria<DashboardLogRegel>(DashboardLogRegel.class);
		criteria.add(Restrictions.eq("logRegel", logRegel));
		return criteria.list(getSession());
	}

}
