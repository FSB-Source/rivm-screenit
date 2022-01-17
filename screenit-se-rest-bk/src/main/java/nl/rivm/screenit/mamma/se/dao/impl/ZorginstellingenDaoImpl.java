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

import java.util.Collection;

import nl.rivm.screenit.mamma.se.dao.ZorginstellingenDao;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ZorginstellingenDaoImpl extends AbstractAutowiredDao implements ZorginstellingenDao
{

	@Override
	public Collection<Instelling> getBKZorginstellingen()
	{
		Criteria crit = getSession().createCriteria(Instelling.class)
			.createAlias("parent", "parent_instelling")
			.add(Restrictions.eq("parent_instelling.organisatieType", OrganisatieType.ZORGINSTELLING))
			.add(Restrictions.or(
				Restrictions.eq("organisatieType", OrganisatieType.MAMMAPOLI),
				Restrictions.eq("organisatieType", OrganisatieType.RADIOLOGIEAFDELING)))
			.setProjection(Projections.distinct(Projections.property("parent")));

		return crit.list();
	}
}
