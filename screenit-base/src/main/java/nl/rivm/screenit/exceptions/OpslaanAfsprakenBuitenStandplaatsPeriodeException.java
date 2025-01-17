package nl.rivm.screenit.exceptions;

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

import java.util.Date;
import java.util.Map;

import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang3.tuple.Pair;

public class OpslaanAfsprakenBuitenStandplaatsPeriodeException extends Exception
{
	private Map<Long, Pair<Date, Date>> afsprakenBuitenStandplaatsPeriodeMap;

	public OpslaanAfsprakenBuitenStandplaatsPeriodeException(Map<Long, Pair<Date, Date>> afsprakenBuitenStandplaatsPeriodeMap)
	{
		super();
		this.afsprakenBuitenStandplaatsPeriodeMap = afsprakenBuitenStandplaatsPeriodeMap;
	}

	@Override
	public String getMessage()
	{
		String afsprakenBuitenStandplaatsPeriodeMelding = "";
		for (Map.Entry<Long, Pair<Date, Date>> entry : afsprakenBuitenStandplaatsPeriodeMap.entrySet())
		{
			afsprakenBuitenStandplaatsPeriodeMelding += getMessage(entry.getValue()) + "\n";
		}
		return afsprakenBuitenStandplaatsPeriodeMelding;
	}

	public String getMessage(Pair<Date, Date> eersteEnLaatsteAfspraakDatums)
	{
		String afsprakenBuitenStandplaatsPeriodeMelding = "";
		Date eersteAfspraakVanaf = eersteEnLaatsteAfspraakDatums.getLeft();
		Date laatsteAfspraakVanaf = eersteEnLaatsteAfspraakDatums.getRight();

		if (eersteAfspraakVanaf != null)
		{
			afsprakenBuitenStandplaatsPeriodeMelding += "Door de gemaakte wijzigingen is een standplaatsperiode korter geworden, waardoor afspraken op de volgende datum(s) buiten de standplaatsperiode vallen: "
				+ DateUtil.formatShortDate(eersteAfspraakVanaf);

			if (laatsteAfspraakVanaf != null && !DateUtil.toLocalDate(eersteAfspraakVanaf).equals(DateUtil.toLocalDate(laatsteAfspraakVanaf)))
			{
				afsprakenBuitenStandplaatsPeriodeMelding += " t/m " + DateUtil.formatShortDate(laatsteAfspraakVanaf);
			}
		}
		return afsprakenBuitenStandplaatsPeriodeMelding;
	}

	public Map<Long, Pair<Date, Date>> getAfsprakenBuitenStandplaatsPeriodeMap()
	{
		return afsprakenBuitenStandplaatsPeriodeMap;
	}
}
