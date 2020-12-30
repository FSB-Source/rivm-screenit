
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

import java.util.List;

import nl.rivm.screenit.main.dao.LocatieDao;
import nl.rivm.screenit.model.colon.Kamer;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.wicket.extensions.markup.html.repeater.util.SortParam;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

@Repository
public class LocatieDaoImpl extends AbstractAutowiredDao implements LocatieDao
{

	@Override
	public List<Kamer> getKamers(int first, int count, SortParam<String> sort)
	{
		BaseCriteria<Kamer> criteria = new BaseCriteria<Kamer>(Kamer.class);
		criteria.add(Restrictions.eq("actief", Boolean.TRUE));
		criteria.createAlias("locatie", "locatie");
		if (sort == null)
		{
			sort = new SortParam<>("locatie.naam", true);
		}
		if (sort.isAscending())
		{
			criteria.addOrder(Order.asc(sort.getProperty()));
		}
		else
		{
			criteria.addOrder(Order.desc(sort.getProperty()));
		}

		return criteria.list(getSession(), first, count);
	}

}
