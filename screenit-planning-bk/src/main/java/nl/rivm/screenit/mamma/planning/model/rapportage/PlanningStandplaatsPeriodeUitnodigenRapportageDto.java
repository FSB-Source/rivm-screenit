package nl.rivm.screenit.mamma.planning.model.rapportage;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PlanningStandplaatsPeriodeUitnodigenRapportageDto
{

	private Long standplaatsPeriodeId;

	private Date uitnodigenTotEnMet;

	private Long uitgenodigdAfspraak = 0L;

	private Long uitgenodigdOpen = 0L;

	private Long uitgenodigdMinderValide = 0L;

	private Long uitgenodigdSuspect = 0L;

	private Long uitgenodigdNaUitstel = 0L;

	private Long uitgesteldAchtervangUitstel = 0L;

	private Long uitgesteldMinderValideUitgewijktUitstel = 0L;

}
