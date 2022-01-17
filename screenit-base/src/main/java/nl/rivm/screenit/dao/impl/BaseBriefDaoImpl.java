package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.BaseBriefDao;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixRegioBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.ScrollableResults;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

@Repository
public class BaseBriefDaoImpl extends AbstractAutowiredDao implements BaseBriefDao
{
	@Override
	public BriefDefinitie getBriefDefinitie(BriefType briefType, Date geldigOp)
	{
		Criteria crit = this.getSession().createCriteria(BriefDefinitie.class);
		crit.add(Restrictions.eq("briefType", briefType));
		crit.add(Restrictions.le("laatstGewijzigd", geldigOp));

		crit.addOrder(Order.desc("laatstGewijzigd"));
		crit.setMaxResults(1);

		return (BriefDefinitie) crit.uniqueResult();
	}

	@Override
	public BriefDefinitie getNieuwsteBriefDefinitie(BriefType briefType)
	{
		Criteria crit = this.getSession().createCriteria(BriefDefinitie.class);
		crit.add(Restrictions.eq("briefType", briefType));

		crit.addOrder(Order.desc("laatstGewijzigd"));
		crit.setMaxResults(1);

		return (BriefDefinitie) crit.uniqueResult();
	}

	@Override
	public ScrollableResults getBriefDefinities(BriefType briefType)
	{
		Criteria crit = this.getSession().createCriteria(BriefDefinitie.class);
		crit.add(Restrictions.eq("briefType", briefType));
		crit.addOrder(Order.asc("laatstGewijzigd"));
		return crit.scroll();
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public <B extends ClientBrief<?, ?, ?>> List<B> getDubbeleAangemaakteBrieven(List<BriefType> vervangenTypes, Client client, Class<B> briefClass)
	{
		Criteria crit = createCriteriaDubbeleAangemaakteBrieven(vervangenTypes, briefClass);
		crit.add(Restrictions.eq("client", client));
		return crit.list();
	}

	@SuppressWarnings({ "unchecked" })
	@Override
	public List<CervixRegioBrief> getDubbeleAangemaakteBrieven(List<BriefType> vervangenTypes, CervixHuisarts arts)
	{
		Criteria crit = createCriteriaDubbeleAangemaakteBrieven(vervangenTypes, CervixRegioBrief.class);
		crit.add(Restrictions.eq("huisarts", arts));
		return crit.list();
	}

	private <B extends Brief> Criteria createCriteriaDubbeleAangemaakteBrieven(List<BriefType> vervangenTypes, Class<B> briefClass)
	{
		Criteria crit = getSession().createCriteria(briefClass);
		if (vervangenTypes.size() > 1)
		{
			crit.add(Restrictions.in("briefType", vervangenTypes));
		}
		else
		{
			crit.add(Restrictions.eq("briefType", vervangenTypes.get(0)));
		}
		crit.add(Restrictions.isNull("mergedBrieven"));
		crit.add(Restrictions.eq("vervangen", Boolean.FALSE));
		return crit;
	}

	@Override
	public boolean clientHeeftOngegenereerdeBriefVanType(BriefType type, Client client, Class<? extends ClientBrief<?, ?, ?>> briefClass)
	{
		Criteria crit = getSession().createCriteria(briefClass);
		crit.add(Restrictions.eq("client", client));
		crit.add(Restrictions.eq("briefType", type));
		crit.add(Restrictions.isNull("mergedBrieven"));
		crit.add(Restrictions.eq("vervangen", Boolean.FALSE));
		crit.setProjection(Projections.id());
		crit.setMaxResults(1);
		return crit.uniqueResult() != null;
	}

	@Override
	public List<ClientBrief<?, ?, ?>> getClientBrieven(Client client)
	{
		Criteria crit = this.getSession().createCriteria(ClientBrief.class);
		crit.add(Restrictions.eq("client", client));

		return crit.list();
	}
}
