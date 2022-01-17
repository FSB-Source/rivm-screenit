package nl.rivm.screenit.main.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.model.EmailConfiguratie;
import nl.rivm.screenit.main.model.OvereenkomstConfiguratie;
import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.model.WachtwoordConfiguratie;
import nl.rivm.screenit.main.model.mamma.IMSConfiguratie;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.dto.UitnodigingCohortDto;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.colon.ColonUitnodigingsinterval;
import nl.rivm.screenit.model.colon.UitnodigingCohort;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.project.Project;

public interface ParameterisatieService
{

	void saveParametrisatieCohort(Account account, List<UitnodigingCohort> oudeCohorten, UitnodigingCohortDto nieuweCohort);

	Parameterisatie loadParameterisatie();

	WachtwoordConfiguratie loadWachtwoordConfiguratie();

	void saveOrUpdateWachtwoordConfiguratie(WachtwoordConfiguratie wachtwoordConfiguratie);

	EmailConfiguratie loadEmailConfiguratie();

	void saveOrUpdateEmailConfiguratie(EmailConfiguratie emailConfiguratie);

	OvereenkomstConfiguratie loadOvereenkomstConfiguratie();

	void saveOrUpdateOvereenkomstConfiguratie(OvereenkomstConfiguratie overeenkomstConfiguratie);

	void saveParameters(Account account, Parameterisatie parameterisatie, Parameterisatie oudParameterisatieObjectParameterisatie, Bevolkingsonderzoek... onderzoek);

	Project getProjectFromPreferences(PreferenceKey key);

	IMSConfiguratie getIMSConfiguratie();

	void saveIMSConfiguratie(Account loggedInAccount, IMSConfiguratie configuratie);

	List<ColonUitnodigingsinterval> getIntervalParameters();

	void saveIntervalParameters(List<ColonUitnodigingsinterval> intervalParameters);
}
