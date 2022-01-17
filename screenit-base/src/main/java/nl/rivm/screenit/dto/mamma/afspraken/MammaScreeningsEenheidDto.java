package nl.rivm.screenit.dto.mamma.afspraken;

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

import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaDuurMinderValideAfspraak;
import nl.rivm.screenit.util.DateUtil;

import java.io.Serializable;
import java.time.LocalTime;

public class MammaScreeningsEenheidDto implements Serializable
{
	public LocalTime minderValidePeriode1Vanaf;

	public LocalTime minderValidePeriode1TotEnMet;

	public LocalTime minderValidePeriode2Vanaf;

	public LocalTime minderValidePeriode2TotEnMet;

	public MammaDuurMinderValideAfspraak duurMinderValideAfspraak;

	public boolean meerdereMammografen;

	public static MammaScreeningsEenheidDto maakScreeningsEenheid(MammaScreeningsEenheid mammaScreeningsEenheid)
	{
		MammaScreeningsEenheidDto screeningsEenheid = new MammaScreeningsEenheidDto();
		screeningsEenheid.minderValidePeriode1Vanaf = DateUtil.toLocalTime(mammaScreeningsEenheid.getMinderValidePeriode1Vanaf());
		screeningsEenheid.minderValidePeriode1TotEnMet = DateUtil.toLocalTime(mammaScreeningsEenheid.getMinderValidePeriode1TotEnMet());
		screeningsEenheid.minderValidePeriode2Vanaf = DateUtil.toLocalTime(mammaScreeningsEenheid.getMinderValidePeriode2Vanaf());
		screeningsEenheid.minderValidePeriode2TotEnMet = DateUtil.toLocalTime(mammaScreeningsEenheid.getMinderValidePeriode2TotEnMet());
		screeningsEenheid.duurMinderValideAfspraak = mammaScreeningsEenheid.getDuurMinderValideAfspraak();
		screeningsEenheid.meerdereMammografen = mammaScreeningsEenheid.getMammografen().size() > 1;

		return screeningsEenheid;
	}
}
