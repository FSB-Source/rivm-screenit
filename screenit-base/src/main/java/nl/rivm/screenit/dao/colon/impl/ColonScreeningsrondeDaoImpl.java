
package nl.rivm.screenit.dao.colon.impl;

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

import nl.rivm.screenit.dao.colon.ColonScreeningsrondeDao;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class ColonScreeningsrondeDaoImpl extends AbstractAutowiredDao implements ColonScreeningsrondeDao
{

	@Override
	public boolean heeftUitslag(ColonScreeningRonde screeningsronde)
	{
		Criteria crit = getSession().createCriteria(IFOBTTest.class);
		crit.add(Restrictions.ne("status", IFOBTTestStatus.ACTIEF));
		crit.add(Restrictions.ne("status", IFOBTTestStatus.VERLOREN));
		crit.add(Restrictions.ne("status", IFOBTTestStatus.VERWIJDERD));
		crit.add(Restrictions.eq("colonScreeningRonde", screeningsronde));
		crit.setProjection(Projections.rowCount());
		Object result = crit.uniqueResult();
		if (result == null)
		{
			return false;
		}
		return ((Long) result).longValue() > 0;
	}

	@Override
	public boolean heeftUitslagOfHeeftGehad(ColonScreeningRonde screeningsronde)
	{
		Criteria crit = getSession().createCriteria(IFOBTTest.class);
		crit.add(Restrictions.ne("status", IFOBTTestStatus.ACTIEF));
		crit.add(Restrictions.ne("status", IFOBTTestStatus.VERLOREN));
		crit.add(Restrictions.eq("colonScreeningRonde", screeningsronde));
		crit.setProjection(Projections.rowCount());
		Object result = crit.uniqueResult();
		if (result == null)
		{
			return false;
		}
		return ((Long) result).longValue() > 0;
	}

}
