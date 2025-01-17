package nl.rivm.screenit.main.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.exception.MagOpslaanException;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeUitnodigenRapportage;

public interface MammaStandplaatsPeriodeService
{
	List<PlanningStandplaatsPeriodeDto> getStandplaatsPeriodesSorted(MammaScreeningsEenheid screeningsEenheid);

	List<MammaStandplaatsPeriode> getStandplaatsPeriodesVoorBulkVerzetten(ScreeningOrganisatie regio);

	void splitsStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriode, InstellingGebruiker ingelogdeInstellingGebruiker);

	void updateSortList(int nieuwVolgnummer, PlanningStandplaatsPeriodeDto item, MammaScreeningsEenheid screeningsEenheid, InstellingGebruiker ingelogdeInstellingGebruiker);

	boolean saveOrUpdateStandplaatsPeriode(PlanningStandplaatsPeriodeDto standplaatsPeriode, InstellingGebruiker ingelogdeInstellingGebruiker);

	MammaStandplaatsRondeUitnodigenRapportage getStandplaatsRondeUitnodigenRapportage(MammaStandplaatsRonde standplaatsRonde);

	List<MammaStandplaats> getStandplaatsenBuitenRegio(IMammaAfspraakWijzigenFilter filter, boolean uitstellen);

	List<MammaScreeningsEenheid> getScreeningEenhedenBuitenRegio(IMammaAfspraakWijzigenFilter filter, boolean uitstellen);

	void magOnthouden(PlanningStandplaatsPeriodeDto standplaatsPeriodeDto, boolean nieuwePrognose, LocalDate nieuweEindDatum, LocalDate vrijgegevenTotEnMet,
		LocalDate uitnodigenTotEnMet, PlanningStandplaatsPeriodeDto volgendeStandplaatsPeriode) throws MagOpslaanException;

	long countAfsprakenTeVerzetten(LocalDate nieuweEindDatum, PlanningStandplaatsPeriodeDto standplaatsPeriodeDto, MammaScreeningsEenheid screeningsEenheid);
}
