package nl.rivm.screenit.mamma.se.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import nl.rivm.screenit.mamma.se.dao.MammaScreeningsEenheidDao;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaScreeningsEenheidDaoImpl extends AbstractAutowiredDao implements MammaScreeningsEenheidDao
{
	@Autowired
	private HibernateService hibernateService;

	@Override
	public MammaScreeningsEenheid getActieveScreeningsEenheidByCode(String seCode)
	{
		Criteria crit = hibernateService.getHibernateSession().createCriteria(MammaScreeningsEenheid.class);
		crit.add(Restrictions.eq("code", seCode));
		crit.add(Restrictions.eq("actief", true));
		return (MammaScreeningsEenheid) crit.uniqueResult();
	}

	@Override
	public String getSeCodeMetIpAdres(String ipAdres)
	{
		Criteria crit = hibernateService.getHibernateSession().createCriteria(MammaScreeningsEenheid.class);
		crit.add(Restrictions.eq("ipAdres", ipAdres));
		crit.add(Restrictions.eq("actief", true));
		crit.setProjection(Projections.property("code"));
		return (String) crit.uniqueResult();
	}
}
