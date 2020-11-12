package nl.rivm.screenit.dao.impl;

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

import java.util.List;

import nl.rivm.screenit.dao.NieuwsDao;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.nieuws.GebruikerNieuwsItem;
import nl.rivm.screenit.model.nieuws.NieuwsItem;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class NieuwsDaoImpl extends AbstractAutowiredDao implements NieuwsDao
{
	private static final long serialVersionUID = 1L;

	@Autowired
	ICurrentDateSupplier iCurrentDateSupplier;

	@Override
	public List<NieuwsItem> getNieuwsItems(boolean inclusiefVerlopenNieuwsItems)
	{
		Criteria criteria = getSession().createCriteria(NieuwsItem.class);
		if (!inclusiefVerlopenNieuwsItems)
		{
			criteria.add(Restrictions.or(Restrictions.isNull("publicerenTot"), Restrictions.gt("publicerenTot", iCurrentDateSupplier.getDate())));
		}

		criteria.addOrder(Order.desc("publicerenVanaf"));
		return criteria.list();
	}

	@Override
	public List<Long> getNieuwsItemIdsGebruiker(Gebruiker gebruiker)
	{
		Criteria criteria = getSession().createCriteria(GebruikerNieuwsItem.class, "gebruikerNieuwsItem");
		criteria.createAlias("nieuwsItem", "nieuwsItem", JoinType.RIGHT_OUTER_JOIN, Restrictions.eq("gebruikerNieuwsItem.gebruiker", gebruiker));

		criteria.add(Restrictions.le("nieuwsItem.publicerenVanaf", iCurrentDateSupplier.getDate()));
		criteria.add(Restrictions.or(Restrictions.isNull("nieuwsItem.publicerenTot"), Restrictions.gt("nieuwsItem.publicerenTot", iCurrentDateSupplier.getDate())));

		criteria.add(Restrictions.or(Restrictions.isNull("gebruikerNieuwsItem.gebruiker"),
			Restrictions.or(Restrictions.isNull("gebruikerNieuwsItem.nietZichtbaarVanaf"),
				Restrictions.or(
					Restrictions.and(Restrictions.isNull("nieuwsItem.gewijzigd"), Restrictions.gtProperty("nieuwsItem.gemaakt", "gebruikerNieuwsItem.nietZichtbaarVanaf")),
					Restrictions.gtProperty("nieuwsItem.gewijzigd", "gebruikerNieuwsItem.nietZichtbaarVanaf")))));

		criteria.addOrder(Order.desc("nieuwsItem.publicerenVanaf"));

		criteria.setProjection(Projections.property("nieuwsItem.id"));
		return criteria.list();
	}
}
