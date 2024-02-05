package nl.rivm.screenit.model.mamma.enums;

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

import nl.rivm.screenit.Constants;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Comparator;

public enum MammaFollowUpTumorGrootteClassificatie
{
	TIS(BigDecimal.valueOf(-1)),
	T1(BigDecimal.valueOf(-1)),
	T1MIC(BigDecimal.valueOf(0.1)),
	T1A(BigDecimal.valueOf(0.5)),
	T1B(BigDecimal.valueOf(1.0)),
	T1C(BigDecimal.valueOf(2.0)),
	T2(BigDecimal.valueOf(5.0)),
	T3(Constants.BK_MAXIMALE_TUMOR_GROOTTE);

	private BigDecimal max;

	MammaFollowUpTumorGrootteClassificatie(BigDecimal max)
	{
		this.max = max;
	}

	public static MammaFollowUpTumorGrootteClassificatie getClassificatie(BigDecimal tumorGrootte)
	{
		if (tumorGrootte.compareTo(BigDecimal.ZERO) >= 0 && tumorGrootte.compareTo(Constants.BK_MAXIMALE_TUMOR_GROOTTE) <= 0)
		{
			return Arrays.stream(MammaFollowUpTumorGrootteClassificatie.values())
				.filter(classificatie -> (tumorGrootte.compareTo(classificatie.max) <= 0))
				.min(Comparator.comparing(MammaFollowUpTumorGrootteClassificatie::getMax)).orElse(null);
		}
		return null;
	}

	public BigDecimal getMax()
	{
		return max;
	}
}
