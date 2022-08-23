package nl.rivm.screenit.batch.jobs.cervix.order.versturenstep;

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

import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieOrderStatus;
import nl.rivm.screenit.model.enums.JobStartParameter;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Component;

@Component
public class CervixOrderVersturenReader extends BaseScrollableResultReader
{

	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		var crit = session.createCriteria(CervixCytologieOrder.class);
		crit.createAlias("uitstrijkje", "uitstrijkje");
		crit.createAlias("uitstrijkje.laboratorium", "laboratorium");
		crit.add(
			Restrictions.or(
				Restrictions.eq("status", CervixCytologieOrderStatus.AANGEMAAKT),
				Restrictions.eq("status", CervixCytologieOrderStatus.MISLUKT)));

		var jobParameters = getStepExecution().getJobExecution().getJobParameters();
		if (jobParameters.toProperties().containsKey(JobStartParameter.CERVIX_ORDER_LABORATORIUM.name()))
		{
			crit.add(Restrictions.eq("laboratorium.id", jobParameters.getLong(JobStartParameter.CERVIX_ORDER_LABORATORIUM.name())));
		}

		return crit;
	}
}
