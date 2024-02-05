package nl.rivm.screenit.dto.mamma.afspraken;

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

import java.util.Date;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaDuurMinderValideAfspraak;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.TimeRange;

@Getter
@Setter
public class MammaScreeningsEenheidDto
{
	private TimeRange minderValidePeriode1;

	private TimeRange minderValidePeriode2;

	private MammaDuurMinderValideAfspraak duurMinderValideAfspraak;

	private boolean enkeleMammograaf;

	public static MammaScreeningsEenheidDto vanEntiteit(MammaScreeningsEenheid screeningsEenheid)
	{
		var screeningsEenheidDto = new MammaScreeningsEenheidDto();
		screeningsEenheidDto.minderValidePeriode1 = maakMindervalidePeriode(screeningsEenheid.getMinderValidePeriode1Vanaf(), screeningsEenheid.getMinderValidePeriode1TotEnMet());
		screeningsEenheidDto.minderValidePeriode2 = maakMindervalidePeriode(screeningsEenheid.getMinderValidePeriode2Vanaf(), screeningsEenheid.getMinderValidePeriode2TotEnMet());
		screeningsEenheidDto.duurMinderValideAfspraak = screeningsEenheid.getDuurMinderValideAfspraak();
		screeningsEenheidDto.enkeleMammograaf = screeningsEenheid.getMammografen().size() <= 1;
		return screeningsEenheidDto;
	}

	private static TimeRange maakMindervalidePeriode(Date vanaf, Date totEnMet)
	{
		return TimeRange.of(DateUtil.toLocalTime(vanaf), DateUtil.toLocalTime(totEnMet));
	}
}
