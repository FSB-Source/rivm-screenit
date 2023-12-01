package nl.rivm.screenit.batch.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;

@Getter
@Setter
@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class UitstelUitnodigingRaportageEntry
{
	private long standplaatsPeriodeId;

	private BriefType briefType;

	public static UitstelUitnodigingRaportageEntry maak(MammaStandplaatsPeriode standplaatsPeriode, MammaUitnodiging uitnodiging)
	{
		return new UitstelUitnodigingRaportageEntry(standplaatsPeriode.getId(), uitnodiging.getBrief().getBriefType());
	}
}