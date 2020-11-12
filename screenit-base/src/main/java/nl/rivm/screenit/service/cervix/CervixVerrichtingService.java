package nl.rivm.screenit.service.cervix;

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

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.cervix.facturatie.CervixVerrichtingenZoekObject;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;

public interface CervixVerrichtingService
{

	List<CervixBoekRegel> getLabVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium bmhkLaboratorium,
		SortState<String> sortState, long first,
		long count);

	long countLabVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium bmhkLaboratorium);

	BigDecimal getLaboratoriumTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium bmhkLaboratorium,
		CervixTariefType tariefType);

	BigDecimal getHuisartsTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie);

	List<CervixBoekRegel> getHuisartsVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie, SortState<String> sortState, long first,
		long count);

	long countHuisartsVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie);

	void verwijderCervixTarief(CervixTarief tarief, Account account, String melding);

	void toevoegenIndexatieTarief(CervixTarief tarief, List<CervixTarief> oudeTarieven, Account account);

	void toevoegenTarief(CervixTarief tarief, Account account);

	CervixTarief getTariefVoorDatum(CervixTariefType tariefType, Date verrichtingsDatum, BMHKLaboratorium bmhkLaboratorium);

	void berekenEinddatumCervixLaboratoriumTarief(BMHKLaboratorium laboratorium);

	CervixBetaalopdracht createBetaalOpdracht(ScreeningOrganisatie screeningOrganisatie, List<CervixBoekRegel> boekregels);

	String getLogMeldingLabTariefVerwijderd(CervixLabTarief labTarief);

	String getLogMeldingHuisartsTariefVerwijderd(CervixHuisartsTarief huisartsTarief);

}
