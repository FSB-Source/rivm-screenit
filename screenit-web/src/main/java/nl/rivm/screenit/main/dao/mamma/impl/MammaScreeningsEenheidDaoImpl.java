package nl.rivm.screenit.main.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.main.dao.mamma.MammaScreeningsEenheidDao;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaScreeningsEenheidDaoImpl extends AbstractAutowiredDao implements MammaScreeningsEenheidDao
{
	@Override
	public List<MammaScreeningsEenheid> getActieveScreeningsEenhedenVoorBeoordelingsEenheden(List<BeoordelingsEenheid> beoordelingsEenheden)
	{
		if (beoordelingsEenheden == null || beoordelingsEenheden.isEmpty())
		{
			return Collections.emptyList();
		}

		Criteria crit = getSession().createCriteria(MammaScreeningsEenheid.class);

		crit.add(Restrictions.in("beoordelingsEenheid", beoordelingsEenheden));
		crit.add(Restrictions.eq("actief", true));

		crit.addOrder(Order.asc("naam"));
		return crit.list();
	}

	@Override
	public List<MammaScreeningsEenheid> zoekScreeningsEenheden(MammaScreeningsEenheid screeningsEenheid, ScreeningOrganisatie regio, int first, int count, String sortProperty,
		boolean asc)
	{
		Criteria crit = createCriteria(screeningsEenheid, regio);
		if (sortProperty != null)
		{
			if (asc)
			{
				crit.addOrder(Order.asc(sortProperty));
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
			}
		}
		crit.setFirstResult(Math.max(first, 0));
		if (count < 0)
		{
			crit.setMaxResults(Integer.MAX_VALUE);
		}
		else
		{
			crit.setMaxResults(count);
		}
		return crit.list();
	}

	private Criteria createCriteria(MammaScreeningsEenheid screeningsEenheid, ScreeningOrganisatie regio)
	{
		Criteria criteria = getSession().createCriteria(MammaScreeningsEenheid.class);
		criteria.createAlias("beoordelingsEenheid", "beoordelingsEenheid");
		criteria.createAlias("beoordelingsEenheid.parent", "parent", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("parent.regio", "regio", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("status", "status", JoinType.LEFT_OUTER_JOIN);

		if (StringUtils.isNotBlank(screeningsEenheid.getCode()))
		{
			criteria.add(Restrictions.ilike("code", screeningsEenheid.getCode(), MatchMode.ANYWHERE));
		}

		if (StringUtils.isNotBlank(screeningsEenheid.getNaam()))
		{
			criteria.add(Restrictions.ilike("naam", screeningsEenheid.getNaam(), MatchMode.ANYWHERE));
		}

		if (regio != null)
		{
			criteria.add(Restrictions.eq("regio.id", regio.getId()));
		}

		if (screeningsEenheid.getActief() != null)
		{
			criteria.add(Restrictions.eq("actief", screeningsEenheid.getActief()));
		}

		if (screeningsEenheid.getBeoordelingsEenheid() != null)
		{
			criteria.add(Restrictions.eq("beoordelingsEenheid", screeningsEenheid.getBeoordelingsEenheid()));
		}

		return criteria;
	}

	@Override
	public long countScreeningsEenheden(MammaScreeningsEenheid screeningsEenheid, ScreeningOrganisatie regio)
	{
		Criteria crit = createCriteria(screeningsEenheid, regio);
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	public List<MammaScreeningsEenheid> getActieveScreeningsEenheden()
	{
		Criteria crit = getSession().createCriteria(MammaScreeningsEenheid.class);
		crit.add(Restrictions.eq("actief", true));
		crit.addOrder(Order.asc("code"));
		return crit.list();
	}

	@Override
	public List<MammaScreeningsEenheid> getActieveScreeningsEenhedenVoorScreeningOrganisatie(ScreeningOrganisatie sessionSO)
	{
		Criteria crit = getSession().createCriteria(MammaScreeningsEenheid.class);
		crit.add(Restrictions.eq("actief", true));
		if (sessionSO != null)
		{
			crit.createAlias("beoordelingsEenheid", "be");
			crit.createAlias("be.parent", "ce");
			crit.createAlias("ce.regio", "regio");
			crit.add(Restrictions.eq("regio.id", sessionSO.getId()));
		}
		crit.addOrder(Order.asc("code"));
		return crit.list();
	}

	@Override
	public long getAantalActieveGekoppeldeOnderzoeken(MammaScreeningsEenheid screeningsEenheid)
	{
		Criteria criteria = getSession().createCriteria(MammaOnderzoek.class, "onderzoek");
		criteria.createAlias("onderzoek.screeningsEenheid", "screeningsEenheid");
		criteria.add(Restrictions.eq("screeningsEenheid.id", screeningsEenheid.getId()));
		criteria.add(Restrictions.eq("onderzoek.isDoorgevoerd", false));
		criteria.setProjection(Projections.rowCount());

		return (long) criteria.uniqueResult();
	}

	@Override
	public long getAantalNietAfgerondeGekoppeldeBeoordelingen(MammaScreeningsEenheid screeningsEenheid)
	{
		Criteria criteria = getSession().createCriteria(MammaOnderzoek.class, "onderzoek");
		criteria.createAlias("onderzoek.beoordelingen", "beoordeling");
		criteria.createAlias("onderzoek.screeningsEenheid", "screeningsEenheid");
		criteria.add(Restrictions.eq("screeningsEenheid.id", screeningsEenheid.getId()));
		criteria.add(Restrictions.not(Restrictions.in("beoordeling.status", MammaBeoordelingStatus.eindStatussen())));

		criteria.setProjection(Projections.rowCount());

		return (long) criteria.uniqueResult();
	}
}
