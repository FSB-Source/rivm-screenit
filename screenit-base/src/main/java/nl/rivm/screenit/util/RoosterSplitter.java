package nl.rivm.screenit.util;

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

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RoosterSplitter
{
	@Autowired
	private OrganisatieParameterService organisatieParameterService;

	public void splitAfspraakslot(Object session, ColonAfspraakslot unsavedObject)
	{
		var startDate = unsavedObject.getVanaf();
		int diffStartEndMinutes = Math.abs(DateUtil.getPeriodeTussenTweeDatums(startDate, unsavedObject.getTot(), ChronoUnit.MINUTES));

		var duurAfspraakInMinuten = organisatieParameterService.getOrganisatieParameter(unsavedObject.getKamer().getIntakelocatie(),
			OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN, 15);
		int berekendAantalBlokken = diffStartEndMinutes / duurAfspraakInMinuten;
		LocalDateTime tot;
		for (int i = 0; i < berekendAantalBlokken; i++)
		{
			ColonAfspraakslot splittedAfspraakslot = unsavedObject.transientClone();
			splittedAfspraakslot.setVanaf(startDate);
			tot = startDate.plusMinutes(duurAfspraakInMinuten);
			splittedAfspraakslot.setTot(tot);
			startDate = tot;
			if (session instanceof Session)
			{
				((Session) session).saveOrUpdate(splittedAfspraakslot);
			}
			else if (session instanceof HibernateService)
			{
				((HibernateService) session).saveOrUpdate(splittedAfspraakslot);
			}
		}
	}

}
