package nl.rivm.screenit.main.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.cervix.cis.CervixCISHistorie;
import nl.rivm.screenit.model.cervix.cis.CervixCISHistorieOngestructureerdRegel;

public class CervixCisHistoryUtil
{
	private CervixCisHistoryUtil()
	{
	}

	public static Map<String, List<CervixCISHistorieOngestructureerdRegel>> getOngestructureerdeRegelsPerRonde(CervixCISHistorie historie, boolean asc)
	{
		Map<String, List<CervixCISHistorieOngestructureerdRegel>> regelsPerRonde = new HashMap<>();
		List<CervixCISHistorieOngestructureerdRegel> regels = new ArrayList<>(historie.getCisHistorieRegels());
		regels.sort((o1, o2) -> {
			int result = 1;
			if (o2.getDatum().before(o1.getDatum()))
			{
				result = -1;
			}

			return result * (asc ? -1 : 1);
		});
		regels.forEach(regel -> {
			if (!regelsPerRonde.containsKey(regel.getRonde()))
			{
				regelsPerRonde.put(regel.getRonde(), new ArrayList<>());
			}
			regelsPerRonde.get(regel.getRonde()).add(regel);
		});
		return regelsPerRonde;
	}

	public static List<String> getOrderdKeys(Map<String, List<CervixCISHistorieOngestructureerdRegel>> regelsPerRonde, boolean asc)
	{
		List<String> rondeList = new ArrayList<>(regelsPerRonde.keySet());
		rondeList.sort(new CervixCisHistoryUtil.CisRondeComparator(asc));
		return rondeList;
	}

	public static class CisRondeComparator implements Comparator<String>, Serializable
	{

		private final boolean ascending;

		public CisRondeComparator(boolean ascending)
		{
			this.ascending = ascending;
		}

		@Override
		public int compare(String o1, String o2)
		{
			int result = 0;
			try
			{
				int rondeO1 = Integer.parseInt(o1);
				int rondeO2 = Integer.parseInt(o2);
				if (rondeO1 == rondeO2)
				{
					result = 0;
				}
				else if (rondeO1 > rondeO2)
				{
					result = -1;
				}
				else
				{
					result = 1;
				}
			}
			catch (NumberFormatException e)
			{
				if (o1.equalsIgnoreCase("memo") && o2.equalsIgnoreCase("bezwaar"))
				{
					result = -1;
				}
				else if (o1.equalsIgnoreCase("memo") && !o2.equalsIgnoreCase("bezwaar"))
				{
					result = 1;
				}
				else if (o1.equalsIgnoreCase("bezwaar"))
				{
					result = 1;
				}
			}

			return result * (ascending ? -1 : 1);
		}

	}
}
