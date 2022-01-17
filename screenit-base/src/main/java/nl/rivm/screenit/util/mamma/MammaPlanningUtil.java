package nl.rivm.screenit.util.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.LocalDate;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.util.DateUtil;

import org.joda.time.DateTime;

public class MammaPlanningUtil
{
	private static int AANTAL_WERKDAGEN_TUSSEN_DATA_GRENS = 5;

	public static boolean datumIsMeerDanVijfWerkdagenVoorDatum(LocalDate teCheckenDatum, LocalDate grensDatum)
	{
		int werkdagenTussenData = DateUtil.getDaysBetweenIgnoreWeekends(new DateTime(DateUtil.toUtilDate(teCheckenDatum)), new DateTime(DateUtil.toUtilDate(grensDatum)), true);
		return !teCheckenDatum.isAfter(grensDatum) && werkdagenTussenData > AANTAL_WERKDAGEN_TUSSEN_DATA_GRENS;
	}

	public static long minimumTijdvak(BigDecimal factor)
	{
		return factor.setScale(0, RoundingMode.HALF_DOWN).multiply(new BigDecimal(Constants.BK_TIJDVAK_MIN)).longValue();
	}
}
