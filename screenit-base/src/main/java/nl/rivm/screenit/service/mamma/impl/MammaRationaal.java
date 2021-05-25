package nl.rivm.screenit.service.mamma.impl;

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

import java.math.BigDecimal;
import java.util.List;

class MammaRationaal implements Comparable<MammaRationaal>
{
	private final BigDecimal deeltal;

	private final BigDecimal deler;

	MammaRationaal(BigDecimal deeltal, BigDecimal deler)
	{
		this.deeltal = deeltal;
		this.deler = deler;
	}

	MammaRationaal()
	{
		deeltal = null;
		deler = null;
	}

	BigDecimal getDeeltal()
	{
		return deeltal;
	}

	BigDecimal getDeler()
	{
		return deler;
	}

	BigDecimal getRatio()
	{
		return getDeeltal().divide(getDeler(), 10, BigDecimal.ROUND_HALF_UP);
	}

	static MammaRationaal getRationaal(List<? extends MammaRationaal> rationaalList)
	{
		BigDecimal totaalDeeltal = BigDecimal.ZERO;
		BigDecimal totaalDeler = BigDecimal.ZERO;
		for (MammaRationaal rationaal : rationaalList)
		{
			totaalDeeltal = totaalDeeltal.add(rationaal.getDeeltal());
			totaalDeler = totaalDeler.add(rationaal.getDeler());
		}
		return new MammaRationaal(totaalDeeltal, totaalDeler);
	}

	@Override
	public int compareTo(MammaRationaal rationaal)
	{
		boolean zero1 = this.getDeler().compareTo(BigDecimal.ZERO) == 0;
		boolean zero2 = rationaal.getDeler().compareTo(BigDecimal.ZERO) == 0;

		if (zero1)
		{
			if (zero2)
			{
				return 0;
			}
			else
			{
				return 1;
			}
		}
		else
		{
			if (zero2)
			{
				return -1;
			}
			else
			{
				return this.getRatio().compareTo(rationaal.getRatio());
			}
		}
	}
}
