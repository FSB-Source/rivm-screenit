package nl.rivm.screenit.batch.jobs.colon.aftergba.overledenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class OverledenReader extends BaseScrollableResultReader
{

	private final ICurrentDateSupplier currentDateDispatcher;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var crit = session.createCriteria(ColonIntakeAfspraak.class);
		crit.createAlias("client", "client");
		crit.createAlias("client.persoon", "persoon");

		crit.add(Restrictions.eq("status", AfspraakStatus.GEPLAND));
		crit.add(Restrictions.ge("startTime", currentDateDispatcher.getDate()));
		crit.add(Restrictions.isNotNull("persoon.overlijdensdatum"));

		return crit;
	}
}
