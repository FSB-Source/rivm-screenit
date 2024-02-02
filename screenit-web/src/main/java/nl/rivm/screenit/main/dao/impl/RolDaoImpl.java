package nl.rivm.screenit.main.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.dao.RolDao;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.collections.CollectionUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class RolDaoImpl extends AbstractAutowiredDao implements RolDao
{

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateRol(Rol rol)
	{
		getSession().saveOrUpdate(rol);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdatePermissie(Permissie permissie)
	{
		getSession().saveOrUpdate(permissie);
	}

	@Override
	public List<Rol> getActieveRollen()
	{
		return createCriteria(null);
	}

	private List<Rol> createCriteria(List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		BaseCriteria<Rol> crit = new BaseCriteria<>(Rol.class);
		crit.add(Restrictions.eq("actief", Boolean.TRUE));
		crit.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
		if (CollectionUtils.isNotEmpty(bevolkingsonderzoeken))
		{
			crit.createAlias("bevolkingsonderzoeken", "bevolkingsonderzoeken");
			crit.add(Restrictions.in("bevolkingsonderzoeken.elements", bevolkingsonderzoeken));
		}
		return crit.list(getSession());
	}

	@Override
	public List<Rol> getActieveRollen(List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		return createCriteria(bevolkingsonderzoeken);
	}

}
