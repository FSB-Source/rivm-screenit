package nl.rivm.screenit.util.mamma;

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

import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.enums.MammaBeperktBeoordeelbaarReden;

public class MammaBeoordelingUtil
{
	public static MammaBeperktBeoordeelbaarReden beperktBeoordeelbaarReden(MammaBeoordeling beoordeling)
	{
		if (beoordeling.getArbitrageLezing() != null && beoordeling.getArbitrageLezing().getBeperktBeoordeelbaarReden() != null)
		{
			return beoordeling.getArbitrageLezing().getBeperktBeoordeelbaarReden();
		}
		else if (beoordeling.getDiscrepantieLezing() != null && beoordeling.getDiscrepantieLezing().getBeperktBeoordeelbaarReden() != null)
		{
			return beoordeling.getDiscrepantieLezing().getBeperktBeoordeelbaarReden();
		}
		else if (beoordeling.getTweedeLezing() != null && beoordeling.getTweedeLezing().getBeperktBeoordeelbaarReden() != null)
		{
			return beoordeling.getTweedeLezing().getBeperktBeoordeelbaarReden();
		}
		else if (beoordeling.getEersteLezing() != null && beoordeling.getEersteLezing().getBeperktBeoordeelbaarReden() != null)
		{
			return beoordeling.getEersteLezing().getBeperktBeoordeelbaarReden();
		}
		else
		{
			return null;
		}
	}

	public static String waaromGeenBeoordelingMogelijk(MammaBeoordeling beoordeling)
	{
		String result = "";
		if (beoordeling.getArbitrageLezing() != null && beoordeling.getArbitrageLezing().getBeperktBeoordeelbaarReden() == MammaBeperktBeoordeelbaarReden.GEEN_BEOORDELING_MOGELIJK)
		{
			result = beoordeling.getArbitrageLezing().getWaaromGeenBeoordelingMogelijk() + "\n\n";
		}
		if (beoordeling.getDiscrepantieLezing() != null
			&& beoordeling.getDiscrepantieLezing().getBeperktBeoordeelbaarReden() == MammaBeperktBeoordeelbaarReden.GEEN_BEOORDELING_MOGELIJK)
		{
			result += beoordeling.getDiscrepantieLezing().getWaaromGeenBeoordelingMogelijk() + "\n\n";
		}
		if (beoordeling.getTweedeLezing() != null && beoordeling.getTweedeLezing().getBeperktBeoordeelbaarReden() == MammaBeperktBeoordeelbaarReden.GEEN_BEOORDELING_MOGELIJK)
		{
			result += beoordeling.getTweedeLezing().getWaaromGeenBeoordelingMogelijk() + "\n\n";
		}
		if (beoordeling.getEersteLezing() != null && beoordeling.getEersteLezing().getBeperktBeoordeelbaarReden() == MammaBeperktBeoordeelbaarReden.GEEN_BEOORDELING_MOGELIJK)
		{
			result += beoordeling.getEersteLezing().getWaaromGeenBeoordelingMogelijk() + "\n\n";
		}
		return result;
	}
}
