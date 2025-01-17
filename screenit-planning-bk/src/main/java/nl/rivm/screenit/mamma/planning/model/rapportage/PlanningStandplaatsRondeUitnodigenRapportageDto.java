package nl.rivm.screenit.mamma.planning.model.rapportage;

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

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaStandplaatsRondeRapportageStatus;

@Getter
@Setter
public class PlanningStandplaatsRondeUitnodigenRapportageDto
{
	private Long standplaatsRondeId;

	private final List<PlanningStandplaatsPeriodeUitnodigenRapportageDto> standplaatsPeriodeUitnodigenRapportages = new ArrayList<>();

	private Long totaalTotaal;

	private Long totaalVervolgRonde;

	private Long totaalEersteRonde;

	private Long totaalDubbeleTijd;

	private Long totaalMinderValide;

	private Long totaalTehuis;

	private Long totaalSuspect;

	private Long uitTeNodigenTotaal;

	private Long uitTeNodigenVervolgRonde;

	private Long uitTeNodigenEersteRonde;

	private Long uitTeNodigenDubbeleTijd;

	private Long uitTeNodigenMinderValide;

	private Long uitTeNodigenTehuis;

	private Long uitTeNodigenSuspect;

	private MammaStandplaatsRondeRapportageStatus status;

}
