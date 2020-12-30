package nl.rivm.screenit.service.mamma.impl;

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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Map;

import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.util.DateUtil;

public class MammaKansberekeningAfspraakContext extends MammaKansberekeningScreeningRondeContext
{

	MammaVerzettenReden verzettenReden;

	LocalDateTime vanaf;

	BriefType briefTypeUitnodiging;

	Integer jaar, maand, uur;

	public MammaKansberekeningAfspraakContext(MammaAfspraak afspraak)
	{
		super(afspraak.getUitnodiging().getScreeningRonde());

		vanaf = DateUtil.toLocalDateTime(afspraak.getVanaf());
		this.jaar = vanaf.getYear();
		this.maand = vanaf.getMonthValue();
		this.uur = vanaf.getHour();
		this.verzettenReden = afspraak.getVerzettenReden();
		this.uitnodiging = afspraak.getUitnodiging();
		this.briefTypeUitnodiging = uitnodiging.getBrief().getBriefType();

		MammaAfspraak laatsteAfspraak = uitnodiging.getLaatsteAfspraak();

		if (laatsteAfspraak != null && MammaAfspraakStatus.NIET_GEANNULEERD.contains(laatsteAfspraak.getStatus()) && !afspraak.equals(laatsteAfspraak))
		{

			Map.Entry<LocalDate, MammaAfspraak> last = afspraakNavigableMap.lastEntry();
			if (last.getValue().getStatus() == MammaAfspraakStatus.GEPLAND)
			{
				afspraakNavigableMap.remove(last.getKey());
			}
		}
	}
}
