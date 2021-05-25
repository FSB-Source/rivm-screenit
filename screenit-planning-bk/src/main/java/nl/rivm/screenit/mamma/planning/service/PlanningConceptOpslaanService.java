package nl.rivm.screenit.mamma.planning.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;

import nl.rivm.screenit.dto.mamma.planning.PlanningConceptMeldingenDto;
import nl.rivm.screenit.exceptions.DryRunException;
import nl.rivm.screenit.exceptions.OpslaanAfsprakenBuitenStandplaatsPeriodeException;
import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;

public interface PlanningConceptOpslaanService
{

	PlanningConceptMeldingenDto opslaan(Long screeningOrganisatieId, boolean runDry) throws OpslaanVerwijderenTijdBlokException, DryRunException, OpslaanAfsprakenBuitenStandplaatsPeriodeException;

	int getAantalAfsprakenTeOntkoppelen(MammaCapaciteitBlok blok, Date vanaf, Date tot, MammaCapaciteitBlokType nieuweBlokType);
}
