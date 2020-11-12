package nl.rivm.screenit.dao.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.dao.colon.HuisartsBerichtDao;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonHuisartsBerichtStatus;
import nl.rivm.screenit.model.Instelling;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class HuisartsBerichtDaoImpl extends AbstractAutowiredDao implements HuisartsBerichtDao
{
	@SuppressWarnings("unchecked")
	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public long countBerichten(ColonHuisartsBericht zoekObject, Instelling regioObject)
	{
		Criteria crit = maakCriteria(zoekObject, regioObject);
		crit.setProjection(Projections.countDistinct("id"));

		List<Long> countResult = crit.list();
		return countResult.get(0).longValue();
	}

	@SuppressWarnings("unchecked")
	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public Iterator<? extends ColonHuisartsBericht> searchBerichten(ColonHuisartsBericht zoekObject, Instelling regioObject, String sortProperty, boolean ascending, int first, int count,
		boolean werklijst)
	{
		Criteria crit = maakCriteria(zoekObject, regioObject);

		if (sortProperty != null)
		{
			if (ascending)
			{
				crit.addOrder(Order.asc(sortProperty));
				if (!StringUtils.equalsIgnoreCase("aanmaakDatum", sortProperty))
				{
					crit.addOrder(Order.asc("aanmaakDatum"));
				}
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
				if (!StringUtils.equalsIgnoreCase("aanmaakDatum", sortProperty))
				{
					crit.addOrder(Order.desc("aanmaakDatum"));
				}
			}
		}
		crit.setFirstResult(first);
		crit.setMaxResults(count);
		return crit.list().iterator();
	}

	private Criteria maakCriteria(ColonHuisartsBericht zoekObject, Instelling regioObject)
	{
		Criteria criteria = getSession().createCriteria(ColonHuisartsBericht.class);
		if (zoekObject.getAanmaakDatum() != null)
		{
			criteria.add(Restrictions.eq("aanmaakDatum", zoekObject.getAanmaakDatum()));
		}
		if (zoekObject.getClient() != null)
		{
			criteria.add(Restrictions.eq("client", zoekObject.getClient()));
		}
		if (zoekObject.getHuisarts() != null)
		{
			criteria.add(Restrictions.eq("huisarts", zoekObject.getHuisarts()));
		}
		if (zoekObject.getBerichtType() != null)
		{
			criteria.add(Restrictions.eq("berichtType", zoekObject.getBerichtType()));
		}
		if (zoekObject.getId() != null)
		{
			criteria.add(Restrictions.eq("id", zoekObject.getId()));
		}
		if (regioObject != null)
		{
			criteria.createAlias("client", "client");
			criteria.createAlias("client.persoon", "persoon");
			criteria.createAlias("persoon.gbaAdres", "gbaAdres");
			criteria.createAlias("gbaAdres.gbaGemeente", "gemeente");
			criteria.add(Restrictions.eq("gemeente.screeningOrganisatie", regioObject));
		}
		return criteria;
	}
}
