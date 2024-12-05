package nl.rivm.screenit.repository.algemeen;

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

import java.util.List;

import nl.rivm.screenit.model.dashboard.DashboardLogRegel;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.dashboard.DashboardType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface DashboardLogRegelRepository extends BaseJpaRepository<DashboardLogRegel>
{
	@Modifying
	@Query("delete from DashboardLogRegel dl where dl.id in (select dl.id from DashboardLogRegel dl join dl.dashboardStatus ds join dl.logRegel lr join lr.logEvent le where ds.type = :dashboardType and le.level = :level)")
	void maakDashboardStatusLeegVoorLandelijk(DashboardType dashboardType, Level level);

	@Modifying
	@Query("delete from DashboardLogRegel dl where dl.dashboardStatus in (select ds.id from DashboardStatus ds where ds.type = :dashboardType)")
	void maakDashboardStatusLeeg(DashboardType dashboardType);

	boolean existsByDashboardStatusType(DashboardType dashboardType);

	List<DashboardLogRegel> findByLogRegel(LogRegel logRegel);

	List<DashboardLogRegel> findByDashboardStatus(DashboardStatus status);
}
