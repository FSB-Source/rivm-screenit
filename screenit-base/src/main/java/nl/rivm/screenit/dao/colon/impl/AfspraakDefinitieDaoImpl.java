
package nl.rivm.screenit.dao.colon.impl;

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

import java.util.List;

import nl.rivm.screenit.dao.colon.AfspraakDefinitieDao;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class AfspraakDefinitieDaoImpl extends AbstractAutowiredDao implements AfspraakDefinitieDao
{

	@Override
	public List<AfspraakDefinitie> getActieDefinities()
	{
		Criteria criteria = getSession().createCriteria(AfspraakDefinitie.class);
		criteria.add(Restrictions.eq("actief", Boolean.TRUE));
		criteria.addOrder(Order.asc("code"));
		return criteria.list();
	}

	@Override
	public List<AfspraakDefinitie> getActieveActieDefinities(Instelling instelling)
	{
		Criteria criteria = getSession().createCriteria(AfspraakDefinitie.class);
		criteria.add(Restrictions.eq("actief", Boolean.TRUE));
		criteria.add(Restrictions.eq("instelling", instelling));
		criteria.addOrder(Order.asc("code"));
		return criteria.list();
	}

}
