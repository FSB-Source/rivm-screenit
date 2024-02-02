package nl.rivm.screenit.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.mamma.MammaBaseStandplaatsPeriodeDao;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseStandplaatsPeriodeDaoImpl extends AbstractAutowiredDao implements MammaBaseStandplaatsPeriodeDao
{

	@Override
	public MammaStandplaatsPeriode getStandplaatsPeriode(MammaScreeningsEenheid screeningsEenheid, Date datum)
	{
		Criteria crit = getSession().createCriteria(MammaStandplaatsPeriode.class, "standplaatsPeriode");
		crit.add(Restrictions.eq("standplaatsPeriode.screeningsEenheid", screeningsEenheid));

		crit.add(Restrictions.le("standplaatsPeriode.vanaf", datum));
		crit.add(Restrictions.ge("standplaatsPeriode.totEnMet", datum));

		return (MammaStandplaatsPeriode) crit.uniqueResult();
	}
}
