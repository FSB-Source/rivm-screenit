package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;
import java.util.List;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.dashboard.DashboardStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;

public interface LogService
{
	void logGebeurtenis(LogGebeurtenis gebeurtenis, Account account, Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, Account account, String melding, Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, Account account, Client client, Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, Account account, Client client, String melding, Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, MammaScreeningsEenheid screeningsEenheid, List<Instelling> dashboardOrganisaties, Account account, Client client,
		String melding, LocalDateTime datumTijd);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, MammaScreeningsEenheid screeningsEenheid, Account account, Client client, String melding, LocalDateTime datumTijd);

	void logGebeurtenis(LogGebeurtenis logGebeurtenis, MammaScreeningsEenheid screeningsEenheid, List<Instelling> dashboardOrganisaties, Client client, String melding);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, LogEvent logEvent, Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, LogEvent logEvent, Account account, Client client, Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, LogEvent logEvent, Account account, Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, Account account, Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, Account account, String melding, Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, Account account, Client client, Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, Account account, Client client, String melding,
		Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, LogEvent logEvent, Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, LogEvent logEvent, Account account, Client client,
		Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, MammaScreeningsEenheid screeningsEenheid, List<Instelling> dashboardOrganisaties, LogEvent logEvent, Account account,
		Client client, LocalDateTime datumTijd, Bevolkingsonderzoek... bevolkingsonderzoeken);

	void logGebeurtenis(LogGebeurtenis gebeurtenis, List<Instelling> dashboardOrganisaties, LogEvent logEvent, Account account, Bevolkingsonderzoek... bevolkingsonderzoeken);

	List<LogRegel> getLogRegels(LoggingZoekCriteria loggingZoekCriteria, int first, int count, SortState<String> sortState);

	List<LogRegel> getLogRegelsVanDashboard(DashboardStatus item, int first, int count, SortState<String> sortState);

	long countLogRegels(LoggingZoekCriteria loggingZoekCriteria);

	long countLogRegelsVanDashboard(DashboardStatus item);

	boolean heeftGeenBestaandeLogregelBinnenPeriode(List<LogGebeurtenis> gebeurtenissen, String bsn, String melding, int dagen);

	boolean heeftGeenBestaandeLogregelBinnenPeriode(List<LogGebeurtenis> gebeurtenissen, String bsn, List<Level> levels, String melding, int dagen);

	boolean verwijderLogRegelsVanDashboards(List<LogRegel> logRegels, InstellingGebruiker ingelogdeGebruiker, LogGebeurtenis logGebeurtenis);
}
