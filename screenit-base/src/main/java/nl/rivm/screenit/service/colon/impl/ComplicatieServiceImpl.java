package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dao.colon.ComplicatieDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.Complicatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.enums.ComplicatieMoment;
import nl.rivm.screenit.service.colon.ComplicatieService;

import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional(propagation = Propagation.SUPPORTS)
public class ComplicatieServiceImpl implements ComplicatieService
{

	@Autowired
	private ComplicatieDao complicatieDao;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	public Iterator<Complicatie> getComplicaties(Complicatie searchObject, List<Long> hierarchieCriteria, int first, int count, SortState<String> sortState)
	{
		return complicatieDao.getComplicaties(searchObject, hierarchieCriteria, first, count, sortState);
	}

	@Override
	public long countComplicaties(Complicatie searchObject, List<Long> hierarchieCriteria)
	{
		return complicatieDao.countComplicaties(searchObject, hierarchieCriteria);
	}

	@Override
	public List<Complicatie> geefAlleNietGekoppeldeComplicaties(Client client, Date date)
	{
		return complicatieDao.geefAlleNietGekoppeldeComplicaties(client, date);
	}

	@Override
	public ComplicatieMoment getCorrecteComplicatieMoment(Date complicatieDatum, MdlVerslag verslag)
	{
		if (verslag != null && verslag.getDatumOnderzoek() != null)
		{
			DateTime onderzoekDatum = new DateTime(verslag.getDatumOnderzoek());
			if (complicatieDatum.before(onderzoekDatum.plusDays(1).toDate()) || complicatieDatum.equals(onderzoekDatum.plusDays(1).toDate()))
			{
				return ComplicatieMoment.BINNEN_24_UUR;
			}
			else if (complicatieDatum.before(onderzoekDatum.plusWeeks(1).toDate()) || complicatieDatum.equals(onderzoekDatum.plusWeeks(1).toDate()))
			{
				return ComplicatieMoment.BINNEN_1_WEEK;
			}
			else if (complicatieDatum.before(onderzoekDatum.plusMonths(1).toDate()) || complicatieDatum.equals(onderzoekDatum.plusMonths(1).toDate()))
			{
				return ComplicatieMoment.BINNEN_1_MAAND;
			}
		}
		return null;
	}

	@Override
	public boolean magComplicatieVastleggen(Date checkDate)
	{
		DateTime stopDatum = DateTimeFormat.forPattern("yyyyMMdd")
			.parseDateTime(preferenceService.getString(PreferenceKey.INTERNAL_COLON_COMPLICATIE_VERWERKING_STOP.name()));
		return checkDate.before(stopDatum.toDate());
	}
}
