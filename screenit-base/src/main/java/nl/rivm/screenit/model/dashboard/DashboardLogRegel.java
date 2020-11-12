
package nl.rivm.screenit.model.dashboard;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import nl.rivm.screenit.model.logging.LogRegel;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "gedeeld", uniqueConstraints = { @UniqueConstraint(name = "dashboardStatusLogRegel", columnNames = { "dashboard_status", "log_regel" }) })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class DashboardLogRegel extends AbstractHibernateObject
{

	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.EAGER, optional = false)
	private DashboardStatus dashboardStatus;

	@ManyToOne(fetch = FetchType.EAGER, optional = false)
	private LogRegel logRegel;

	public DashboardStatus getDashboardStatus()
	{
		return dashboardStatus;
	}

	public void setDashboardStatus(DashboardStatus dashboardStatus)
	{
		this.dashboardStatus = dashboardStatus;
	}

	public LogRegel getLogRegel()
	{
		return logRegel;
	}

	public void setLogRegel(LogRegel logRegel)
	{
		this.logRegel = logRegel;
	}
}
