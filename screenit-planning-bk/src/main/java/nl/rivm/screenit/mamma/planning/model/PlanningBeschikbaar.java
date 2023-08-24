package nl.rivm.screenit.mamma.planning.model;

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

import java.math.BigDecimal;

import lombok.Getter;

import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;

@Getter
public class PlanningBeschikbaar
{
	private BigDecimal totaal;

	private BigDecimal totaalTehuis;

	public PlanningBeschikbaar()
	{
		clear();
	}

	public void add(PlanningBeschikbaar beschikbaar)
	{
		totaal = totaal.add(beschikbaar.totaal);
		totaalTehuis = totaalTehuis.add(beschikbaar.totaalTehuis);
	}

	public void add(BigDecimal totaal, MammaCapaciteitBlokType blokType)
	{
		switch (blokType)
		{
		case TEHUIS:
			totaalTehuis = totaalTehuis.add(totaal);

		case REGULIER:
			this.totaal = this.totaal.add(totaal);
			break;
		case GEEN_SCREENING:
			break;
		default:
			throw new IllegalArgumentException(blokType + " niet toegestaan.");
		}
	}

	public BigDecimal getTotaalRegulier()
	{
		return getTotaal().subtract(getTotaalTehuis());
	}

	public void clear()
	{
		totaal = BigDecimal.ZERO;
		totaalTehuis = BigDecimal.ZERO;
	}
}
