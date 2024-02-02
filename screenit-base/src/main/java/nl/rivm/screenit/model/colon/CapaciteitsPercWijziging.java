package nl.rivm.screenit.model.colon;

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

import java.io.Serializable;
import java.math.BigDecimal;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CapaciteitsPercWijziging implements Serializable
{

	private Long ilUgId;

	private Long ilId;

	private String intakelocatie;

	private Long ugId;

	private String uitnodigingsgebied;

	private Integer oudCapPer;

	private BigDecimal oudBerekendeIntakes;

	private BigDecimal oudIntakesProg;

	private Integer nieuwCapPer;

	private BigDecimal nieuwBerekendeIntakes;

	private BigDecimal nieuwIntakesProg;

	private Integer oudAdhPer;

	private Integer nieuwAdhPer;

	public BigDecimal getVerschilOud()
	{
		if (oudIntakesProg != null)
		{
			return oudIntakesProg.subtract(oudBerekendeIntakes);
		}
		return null;
	}

	public BigDecimal getVerschilNieuw()
	{
		if (nieuwIntakesProg != null)
		{
			return nieuwIntakesProg.subtract(nieuwBerekendeIntakes);
		}
		return null;
	}

	@Override
	public boolean equals(Object other)
	{
		CapaciteitsPercWijziging wijziging = (CapaciteitsPercWijziging) other;
		return ilId.equals(wijziging.ilId) && ugId.equals(wijziging.ugId);
	}

}
