
package nl.rivm.screenit.main.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.dao.ClientContactDao;
import nl.rivm.screenit.model.ClientContact;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

@Repository
public class ClientContactDaoImpl extends AbstractAutowiredDao implements ClientContactDao
{

	@Override
	public Long countClientContactenMetOpmerking(Long clientId)
	{
		Criteria crit = this.getSession().createCriteria(ClientContact.class);
		crit.add(Restrictions.eq("client.id", clientId));
		crit.add(Restrictions.isNotNull("opmerking"));

		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}
}
