package nl.rivm.screenit.exceptions;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.util.DateUtil;

import java.util.Date;
import java.util.Map;

public class OpslaanAfsprakenBuitenStandplaatsPeriodeException extends Exception
{
	private Map<Long, Date[]> afsprakenBuitenStandplaatsPeriodeMap;

	public OpslaanAfsprakenBuitenStandplaatsPeriodeException(Map<Long, Date[]> afsprakenBuitenStandplaatsPeriodeMap)
	{
		super();
		this.afsprakenBuitenStandplaatsPeriodeMap = afsprakenBuitenStandplaatsPeriodeMap;
	}

	@Override
	public String getMessage()
	{
		String afsprakenBuitenStandplaatsPeriodeMelding = "";
		for (Map.Entry<Long, Date[]> entry : afsprakenBuitenStandplaatsPeriodeMap.entrySet())
		{
			afsprakenBuitenStandplaatsPeriodeMelding += getMessage(entry.getValue()) + "\n";
		}
		return afsprakenBuitenStandplaatsPeriodeMelding;
	}

	public String getMessage(Date[] eersteEnLaatsteAfspraakDatums)
	{
		String afsprakenBuitenStandplaatsPeriodeMelding = "";
		Date eersteAfspraakVanaf = eersteEnLaatsteAfspraakDatums[0];
		Date laatsteAfspraakVanaf = eersteEnLaatsteAfspraakDatums[1];

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

	public Map<Long, Date[]> getAfsprakenBuitenStandplaatsPeriodeMap()
	{
		return afsprakenBuitenStandplaatsPeriodeMap;
	}
}
