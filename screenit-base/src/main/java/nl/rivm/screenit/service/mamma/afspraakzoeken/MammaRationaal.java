package nl.rivm.screenit.service.mamma.afspraakzoeken;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

public class MammaRationaal implements Comparable<MammaRationaal>
{
	private final BigDecimal deeltal;

	private final BigDecimal deler;

	public MammaRationaal(BigDecimal deeltal, BigDecimal deler)
	{
		this.deeltal = deeltal;
		this.deler = deler;
	}

	protected MammaRationaal()
	{
		deeltal = null;
		deler = null;
	}

	public BigDecimal getDeeltal()
	{
		return deeltal;
	}

	public BigDecimal getDeler()
	{
		return deler;
	}

	public BigDecimal getRatio()
	{
		return getDeeltal().divide(getDeler(), 10, RoundingMode.HALF_UP);
	}

	public String getRatioTekst()
	{
		return delerIsZero() ? "oneindig" : getRatio().toString();
	}

	private boolean delerIsZero()
	{
		return getDeler().compareTo(BigDecimal.ZERO) == 0;
	}

	public static MammaRationaal getRationaal(List<? extends MammaRationaal> rationaalList)
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
	public int compareTo(MammaRationaal other)
	{
		boolean thisZeroDeler = delerIsZero();
		boolean otherZeroDeler = other.delerIsZero();

		if (thisZeroDeler)
		{
			if (otherZeroDeler)
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
			if (otherZeroDeler)
			{
				return -1;
			}
			else
			{
				return this.getRatio().compareTo(other.getRatio());
			}
		}
	}
}
