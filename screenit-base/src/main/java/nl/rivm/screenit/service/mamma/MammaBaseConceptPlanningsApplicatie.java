package nl.rivm.screenit.service.mamma;

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

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Date;
import java.util.NavigableSet;

import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsEenheidMetaDataDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsOrganisatieDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningStatusDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningVerzetClientenDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningWeekDto;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
public interface MammaBaseConceptPlanningsApplicatie
{

	void sendPostcodeReeks(MammaPostcodeReeks postcodeReeks, boolean isNieuw);

	NavigableSet<String> getUncoveredPostcodes(ScreeningOrganisatie screeningOrganisatie);

	void deletePostcodeReeks(MammaPostcodeReeks postcodeReeks);

	void sendStandplaats(MammaStandplaats standplaats, boolean isNieuw);

	void sendScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid, boolean isNieuw);

	PlanningWeekDto getWeek(MammaScreeningsEenheid screeningEenheid, Date start);

	String getAfspraakDrempelOverzichtStandplaats(long standplaatsId);

	String getAfspraakDrempelOverzichtScreeningsOrganisatie(long screeningsOrganisatieId);

	PlanningScreeningsEenheidMetaDataDto getScreeningsEenheidMetaData(MammaScreeningsEenheid screeningEenheid);

	void sendCapaciteitBlok(PlanningCapaciteitBlokDto blok, boolean isNieuw, InstellingGebruiker ingelogdeInstellingGebruiker);

	String deleteCapaciteitBlok(PlanningCapaciteitBlokDto blok, InstellingGebruiker ingelogdeInstellingGebruiker);

	PlanningStandplaatsPeriodeDto[] getStandplaatsPeriodesSorted(MammaScreeningsEenheid screeningsEenheid);

	void changeRoute(PlanningStandplaatsPeriodeDto item, MammaScreeningsEenheid screeningsEenheid, InstellingGebruiker ingelogdeInstellingGebruiker);

	void splitsStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriodeDto, InstellingGebruiker ingelogdeInstellingGebruiker);

	void sendAfspraakDrempelStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriode, InstellingGebruiker ingelogdeInstellingGebruiker);

	Long[] getStandplaatsenZonderRoute(ScreeningOrganisatie screeningOrganisatie);

	Long[] getStandplaatsenMetRoute(ScreeningOrganisatie screeningOrganisatie);

	PlanningConceptMeldingenDto saveConcept(InstellingGebruiker ingelogdeInstellingGebruiker, boolean runDry);

	void conceptAnnuleren(InstellingGebruiker ingelogdeInstellingGebruiker);

	void sendBlokkade(MammaBlokkade blokkade, boolean isNieuw);

	int getAantalAfsprakenOpBlok(PlanningCapaciteitBlokDto blokDto, boolean toDelete);

	void herhaalWeek(MammaScreeningsEenheid screeningsEenheidVan, MammaScreeningsEenheid screeningsEenheidNaar, LocalDate teHerhalenWeek, LocalDate herhalenVanafWeek,
		LocalDate herhalenTotEnMetWeek, InstellingGebruiker ingelogdeInstellingGebruiker);

	void updateScreeningsOrganisatie(PlanningScreeningsOrganisatieDto screeningsOrganisatieDto);

	Long[] getConceptGewijzigdDoor(ScreeningOrganisatie screeningOrganisatie);

	Date getPlannenTotEnMetDatum();

	void verzetClienten(PlanningVerzetClientenDto verzetClientenDto);

	PlanningStatusDto getStatus();

	void kopieerDag(MammaScreeningsEenheid bronScreeningsEenheid, MammaScreeningsEenheid doelScreeningsEenheid, LocalDate bronDag, LocalTime bronVanTijd, LocalTime bronTotTijd,
		LocalDate doelDag, InstellingGebruiker ingelogdeInstellingGebruiker);

}
