package nl.rivm.screenit.batch.jobs.cervix.order.aanmaakstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.batch.jobs.helpers.BaseScrollableResultReader;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.batch.core.JobParameters;

public class CervixOrderAanmaakReader extends BaseScrollableResultReader
{
	
	@Override
	public Criteria createCriteria(StatelessSession session) throws HibernateException
	{
		Criteria crit = session.createCriteria(CervixUitstrijkje.class, "uitstrijkje");
		crit.createAlias("uitstrijkje.labformulier", "labformulier");
        crit.createAlias("laboratorium", "laboratorium");
		crit.add(Restrictions.ne("uitstrijkje.uitstrijkjeStatus", CervixUitstrijkjeStatus.NIET_ONTVANGEN));
		crit.add(Restrictions.ne("uitstrijkje.uitstrijkjeStatus", CervixUitstrijkjeStatus.NIET_ANALYSEERBAAR));

		crit.add(Restrictions.isNull("cytologieOrder"));
		crit.add(Restrictions.eq("labformulier.status", CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE));

		JobParameters jobParameters = getStepExecution().getJobExecution().getJobParameters();
		if (jobParameters.toProperties().containsKey(Constants.CERVIX_ORDER_JOB_LABORATORIUM_PARAMETER))
		{
			crit.add(Restrictions.eq("laboratorium.id", jobParameters.getLong(Constants.CERVIX_ORDER_JOB_LABORATORIUM_PARAMETER)));
		}
		return crit;
	}
}
