package nl.rivm.screenit.service.colon.impl;

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

import java.time.temporal.ChronoUnit;
import java.util.Date;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.enums.ComplicatieMoment;
import nl.rivm.screenit.service.colon.ComplicatieService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ComplicatieServiceImpl implements ComplicatieService
{

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public ComplicatieMoment getCorrecteComplicatieMoment(Date complicatieDatum, MdlVerslag verslag)
	{
		if (verslag != null && verslag.getDatumOnderzoek() != null)
		{
			var onderzoekDatum = verslag.getDatumOnderzoek();
			if (!DateUtil.compareAfter(complicatieDatum, DateUtil.plusDagen(onderzoekDatum, 1)))
			{
				return ComplicatieMoment.BINNEN_24_UUR;
			}
			else if (!DateUtil.compareAfter(complicatieDatum, DateUtil.plusTijdseenheid(onderzoekDatum, 1, ChronoUnit.WEEKS)))
			{
				return ComplicatieMoment.BINNEN_1_WEEK;
			}
			else if (!DateUtil.compareAfter(complicatieDatum, DateUtil.plusTijdseenheid(onderzoekDatum, 1, ChronoUnit.MONTHS)))
			{
				return ComplicatieMoment.BINNEN_1_MAAND;
			}
		}
		return null;
	}

	@Override
	public boolean magComplicatieVastleggen(Date checkDate)
	{
		var stopDatum = DateUtil.parseDateForPattern(preferenceService.getString(PreferenceKey.INTERNAL_COLON_COMPLICATIE_VERWERKING_STOP.name()), Constants.DATE_FORMAT_YYYYMMDD);
		return checkDate.before(stopDatum);
	}
}
