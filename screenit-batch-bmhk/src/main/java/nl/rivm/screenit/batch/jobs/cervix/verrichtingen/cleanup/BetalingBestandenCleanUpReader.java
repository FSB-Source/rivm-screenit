package nl.rivm.screenit.batch.jobs.cervix.verrichtingen.cleanup;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import java.util.Date;

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;

public class BetalingBestandenCleanUpReader extends BaseScrollableResultReader
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{

		Criteria crit = session.createCriteria(CervixBetaalopdracht.class);
		crit.add(Restrictions.isNotNull("sepaSpecificatiePdf"));
		crit.add(Restrictions.isNotNull("sepaDocument"));
		crit.add(Restrictions.le("statusDatum", getDateTwoYearsBack()));
		crit.add(Restrictions.or(Restrictions.eq("status", BestandStatus.VERWERKT), Restrictions.eq("status", BestandStatus.GEARCHIVEERD)));
		return crit;
	}

	private Date getDateTwoYearsBack()
	{
		DateTime dateTime = currentDateSupplier.getDateTimeMidnight();
		dateTime = dateTime.minusYears(2);
		return dateTime.toDate();
	}
}
