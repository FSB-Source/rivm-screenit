package nl.rivm.screenit.dto.mamma.afspraken;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;

public class MammaStandplaatsPeriodeMetAfstandDto implements Serializable
{
	private final Long standplaatsPeriodeId;

	private final Double afstand;

	public MammaStandplaatsPeriodeMetAfstandDto(Long standplaatsPeriodeId, Double afstand)
	{
		this.standplaatsPeriodeId = standplaatsPeriodeId;
		this.afstand = afstand;
	}

	public Long getStandplaatsPeriodeId()
	{
		return standplaatsPeriodeId;
	}

	public Double getAfstand()
	{
		return afstand;
	}

	public boolean isOnbekendeAfstand()
	{
		return afstand.equals(MammaKandidaatAfspraakDto.ONBEKENDE_AFSTAND);
	}
}
