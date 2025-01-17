package nl.rivm.screenit.mamma.planning.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.util.NavigableSet;
import java.util.Set;

import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaatsRonde;
import nl.rivm.screenit.mamma.planning.model.rapportage.PlanningStandplaatsPeriodeUitnodigenRapportageDto;
import nl.rivm.screenit.mamma.planning.model.rapportage.PlanningUitnodigenRapportageDto;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;

public interface PlanningUitnodigenService
{
	void uitnodigen(PlanningStandplaatsRonde standplaatsRonde, Set<PlanningClient> openUitnodigingClientSet, NavigableSet<PlanningClient> afspraakUitnodigingClientSet,
					PlanningUitnodigenRapportageDto rapportageDto, PlanningUitnodigingContext context);

	void achtervangUitstel(PlanningStandplaatsRonde standplaatsRonde, Set<PlanningClient> uitTeStellenClientSet, PlanningUitnodigenRapportageDto rapportageDto);

	void minderValideUitwijkUitstel(PlanningStandplaatsRonde planningStandplaatsRonde, Set<PlanningClient> uitTeStellenClientSet, PlanningUitnodigenRapportageDto rapportageDto);

	void clear();

	PlanningStandplaatsPeriodeUitnodigenRapportageDto getStandplaatsPeriodeUitnodigenRapportage(PlanningUitnodigenRapportageDto rapportageDto,
																								MammaStandplaatsPeriode standplaatsPeriode);
}
