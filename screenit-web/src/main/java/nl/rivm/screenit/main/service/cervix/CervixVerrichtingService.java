package nl.rivm.screenit.main.service.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dto.cervix.facturatie.CervixVerrichtingenZoekObject;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;

import org.springframework.data.domain.Sort;

public interface CervixVerrichtingService
{

	List<CervixBoekRegel> getLabVerrichtingen(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium laboratorium,
		Sort sort, long first, long count);

	long countLabVerrichtingen(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium laboratorium);

	BigDecimal getLaboratoriumTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium laboratorium,
		CervixTariefType tariefType);

	BigDecimal getHuisartsTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie);

	List<CervixBoekRegel> getHuisartsVerrichtingen(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie, Sort sort, long first, long count);

	long countHuisartsVerrichtingen(CervixVerrichtingenZoekObject verrichtingenZoekObject, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie);

	CervixBetaalopdracht createBetaalOpdracht(ScreeningOrganisatie screeningOrganisatie, List<CervixBoekRegel> boekregels);

	List<CervixLabTarief> getLabTarieven(BMHKLaboratorium laboratorium, long first, long count, Sort sort);

	Long countLabTarieven(BMHKLaboratorium laboratorium);

	boolean heeftLaboratoriumTarief(CervixLabTarief tarief);

	boolean heeftHuisartsTarief(Date datum);

	Long countHuisartsTarieven();

	CervixLabTarief getLatestLabTarief(BMHKLaboratorium lab);

	CervixHuisartsTarief getLatestHuisartsTarief();

	List<CervixHuisartsTarief> getHuisartsTarieven(long first, long count, Sort sort);
}
