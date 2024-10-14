package nl.rivm.screenit.service;

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

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.dashboard.DashboardLogRegel;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.dashboard.DashboardType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogRegel;

public interface DashboardService
{
	void updateDashboard(LogRegel logRegel, List<Instelling> dashboardOrganisaties);

	boolean updateLogRegelMetDashboardStatus(LogRegel logregel, String gebruikersnaam, DashboardStatus dashboardStatus);

	List<DashboardStatus> getDashboardStatussen(DashboardType item);

	boolean verwijderLogRegelsVanDashboards(List<LogGebeurtenis> gebeurtenissen, String bsn, String melding);

	boolean verwijderLogRegelsVanDashboards(List<LogRegel> logRegels);

	Level getHoogsteLevelDashboardItems(Instelling ingelogdVoorOrganisatie, List<Bevolkingsonderzoek> bevolkingsOnderzoeken);

	List<DashboardStatus> getListOfDashboardStatussen(Instelling ingelogdVoorOrganisatie, List<Bevolkingsonderzoek> bevolkingsOnderzoeken, List<Level> loggingLevels);

	List<DashboardLogRegel> getDashboardLogRegelMetLogRegel(LogRegel logRegel);
}
