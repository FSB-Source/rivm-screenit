package nl.rivm.screenit.main.util;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.BigDecimalUtil;
import org.apache.commons.lang.StringEscapeUtils;

import java.math.BigDecimal;

public class ExportToXslUtil
{

	public static String getStandplaatsLocatieString(MammaStandplaatsLocatie standplaatsLocatie)
	{
		return AdresUtil.getVolledigeAdresString(standplaatsLocatie);
	}

	public static String getIntervalString(BigDecimal interval)
	{
		return interval != null ? BigDecimalUtil.roundToNearestHalf(interval).toString() : "";
	}

	public static String getCsvValue(String value)
	{
		return StringEscapeUtils.escapeCsv(value);
	}
}
