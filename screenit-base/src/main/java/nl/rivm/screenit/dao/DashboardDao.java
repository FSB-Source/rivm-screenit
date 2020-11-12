package nl.rivm.screenit.dao;

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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.dashboard.DashboardLogRegel;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.dashboard.DashboardType;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.logging.LogRegel;

public interface DashboardDao
{
	
	List<DashboardStatus> getDashboardStatussen(DashboardType item);

	List<DashboardStatus> getListOfDashboardStatussen(ToegangLevel toegang);

	void maakDashboardStatusLeeg(DashboardType dashboardType);

	Date getDateTimeLastLogRegel(DashboardType dashboardType);

	List<DashboardStatus> getListOfDashboardStatussen(Instelling ingelogdVoorOrganisatie);

	List<DashboardLogRegel> getDashboardLogRegelMetLogRegel(LogRegel logRegel);
}
