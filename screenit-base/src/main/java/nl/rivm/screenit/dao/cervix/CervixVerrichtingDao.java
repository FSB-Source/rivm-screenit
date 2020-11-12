package nl.rivm.screenit.dao.cervix;

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

import nl.rivm.screenit.dto.cervix.facturatie.CervixBetalingsZoekObject;
import nl.rivm.screenit.dto.cervix.facturatie.CervixVerrichtingenZoekObject;
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
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;

public interface CervixVerrichtingDao
{
	
	List<CervixBoekRegel> getVerrichtingenVoorBetaling(CervixBetalingsZoekObject zoekObject, SortState<String> sortState, long first, long count);

	List<CervixBoekRegel> getLabVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium bmhkLaboratorium,
		SortState<String> sortState, long first,
		long count);

	long countLabVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium bmhkLaboratorium);

	CervixTarief getGeldigForDatumTarief(CervixTariefType tariefType, Date date, BMHKLaboratorium bmhkLaboratorium);

	List<CervixBoekRegel> getHuisartsVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie,
		SortState<String> sortState, long first,
		long count);

	long countHuisartsVerrichtingen(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie);

	List<CervixHuisartsTarief> getCervixHuisartsTarieven(CervixHuisartsTarief tarief, long first, long count, SortState<String> sortState);

	Long countCervixHuisartsTarieven(CervixHuisartsTarief tarief);

	CervixHuisartsTarief getLatestCervixHuisartsTarief();

	CervixLabTarief getLatestCervixLabTarief(BMHKLaboratorium lab);

	CervixHuisartsTarief getCervixHuisartsTarief(Date datum);

	CervixLabTarief getCervixLaboratoriumTarief(CervixLabTarief tarief);

	List<CervixHuisartsTarief> getCervixHuisartsTarievenZonderEinddatum();

	List<CervixLabTarief> getCervixLabTarievenZonderEinddatum(BMHKLaboratorium lab);

	List<CervixLabTarief> getCervixLabTarieven(CervixLabTarief zoekobject, long first, long count, SortState<String> sortState);

	Long countCervixLabTarieven(CervixLabTarief zoekobject);

	List<CervixBetaalopdracht> getCervixBetaalopdrachten(ScreeningOrganisatie organisatie, SortState<String> sortState, long first, long count);

	Long countCervixBetaalOpdrachten(ScreeningOrganisatie organisatie);

	List<CervixBetaalopdracht> getVandaagGemaakteBetaalOpdrachten();

	BigDecimal getLaboratoriumTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, BMHKLaboratorium bmhkLaboratorium,
		CervixTariefType tariefType);

	BigDecimal getHuisartsTotaalBedrag(CervixVerrichtingenZoekObject verrichtingenCriteria, ScreeningOrganisatie screeningOrganisatie, CervixHuisarts huisarts,
		CervixHuisartsLocatie huisartsLocatie);

	List<CervixVerrichting> getVerrichtingenVoorTarief(Long oudTariefId, CervixTarief nieuweTarief, CervixTariefType tariefType);

	List<CervixTarief> getHuisartsTarievenTussen(Date vanafDatum, Date totEnMetDatum);

	List<CervixTarief> getLabTarievenTussen(BMHKLaboratorium lab, Date vanafDatum, Date totEnMetDatum);

}
