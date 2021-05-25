package nl.rivm.screenit.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.mamma.MammaBaseBlokkadeDao;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseBlokkadeDaoImpl extends AbstractAutowiredDao implements MammaBaseBlokkadeDao
{
	public BaseCriteria<MammaBlokkade> baseCriteria(MammaBlokkade zoekObject)
	{
		BaseCriteria<MammaBlokkade> crit = new BaseCriteria<>(MammaBlokkade.class, "blokkade");
		crit.alias("blokkade.screeningsEenheid", "screeningsEenheid", JoinType.LEFT_OUTER_JOIN);
		crit.alias("blokkade.standplaats", "standplaats", JoinType.LEFT_OUTER_JOIN);
		crit.alias("blokkade.regio", "regio", JoinType.LEFT_OUTER_JOIN);
		crit.add(Restrictions.or(
			Restrictions.eq("screeningsEenheid.actief", true),
			Restrictions.eq("standplaats.actief", true),
			Restrictions.eq("regio.actief", true)));
		if (zoekObject.getActief() != null)
		{
			crit.add(Restrictions.eq("blokkade.actief", zoekObject.getActief()));

		}
		if (zoekObject.getType() != null)
		{
			crit.add(Restrictions.eq("blokkade.type", zoekObject.getType()));
		}
		if (zoekObject.getRegio() != null)
		{
			crit.createAlias("screeningsEenheid.beoordelingsEenheid", "be", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("be.parent", "ce", JoinType.LEFT_OUTER_JOIN);
			crit.createAlias("ce.regio", "screeningsEenheidRegio", JoinType.LEFT_OUTER_JOIN);

			crit.add(Restrictions.or(
				Restrictions.eq("blokkade.regio.id", zoekObject.getRegio().getId()),
				Restrictions.eq("screeningsEenheidRegio.id", zoekObject.getRegio().getId()),
				Restrictions.eq("standplaats.regio.id", zoekObject.getRegio().getId())));
		}
		if (zoekObject.getScreeningsEenheid() != null)
		{
			crit.add(Restrictions.eq("blokkade.screeningsEenheid.id", zoekObject.getScreeningsEenheid().getId()));
		}
		if (zoekObject.getStandplaats() != null)
		{
			crit.add(Restrictions.eq("blokkade.standplaats.id", zoekObject.getStandplaats().getId()));
		}
		Conjunction and = Restrictions.conjunction();
		crit.add(and);
		if (zoekObject.getVanaf() != null)
		{
			and.add(Restrictions.ge("blokkade.totEnMet", zoekObject.getVanaf()));
		}
		if (zoekObject.getTotEnMet() != null)
		{
			and.add(Restrictions.le("blokkade.vanaf", zoekObject.getTotEnMet()));
		}
		return crit;
	}

	@Override
	public int countBlokkades(MammaBlokkade zoekObject)
	{
		return baseCriteria(zoekObject).count(getSession());
	}

	@Override
	public List<MammaBlokkade> getBlokkades(MammaBlokkade zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		return baseCriteria(zoekObject).list(getSession(), new ListCriteria(first, count, sortProperty, asc));
	}

	@Override
	public List<MammaBlokkade> getOverlappendeBlokkades(MammaBlokkade blokkade)
	{
		Criterion vanafTussenBeginEnEindDatum = Restrictions.and(Restrictions.le("blokkade.vanaf", blokkade.getVanaf()),
			Restrictions.ge("blokkade.totEnMet", blokkade.getVanaf()));
		Criterion totEndMetTussenBeginEnEindDatum = Restrictions.and(Restrictions.le("blokkade.vanaf", blokkade.getTotEnMet()),
			Restrictions.ge("blokkade.totEnMet", blokkade.getTotEnMet()));
		Criterion overkoepelend = Restrictions.and(Restrictions.gt("blokkade.vanaf", blokkade.getVanaf()),
			Restrictions.lt("blokkade.totEnMet", blokkade.getTotEnMet()));

		final BaseCriteria<MammaBlokkade> crit = new BaseCriteria<>(MammaBlokkade.class, "blokkade");
		crit.add(Restrictions.or(vanafTussenBeginEnEindDatum, totEndMetTussenBeginEnEindDatum, overkoepelend));
		crit.add(Restrictions.eq("type", blokkade.getType()));
		switch (blokkade.getType())
		{
		case SCREENINGS_EENHEID:
			crit.add(Restrictions.eq("screeningsEenheid", blokkade.getScreeningsEenheid()));
			break;
		case SCREENINGS_ORGANISATIE:
			crit.add(Restrictions.eq("regio", blokkade.getRegio()));
			break;
		case STANDPLAATS:
			crit.add(Restrictions.eq("standplaats", blokkade.getStandplaats()));
			break;
		}
		crit.add(Restrictions.eq("actief", blokkade.getActief()));
		if (blokkade.getId() != null)
		{
			crit.add(Restrictions.ne("id", blokkade.getId()));
		}

		return crit.list(getSession());
	}

	@Override
	public List<MammaBlokkade> getActieveBlokkadesVoorSE(MammaStandplaats standplaats, MammaScreeningsEenheid se, Date dag)
	{
		Criteria criteria = getSession().createCriteria(MammaBlokkade.class, "blokkade");
		criteria.add(Restrictions.eq("blokkade.actief", true));
		criteria.add(Restrictions.le("blokkade.vanaf", dag));
		criteria.add(Restrictions.ge("blokkade.totEnMet", dag));

		Disjunction or = Restrictions.disjunction();
		criteria.add(or);

		or.add(Restrictions.eq("blokkade.screeningsEenheid.id", se.getId()));

		Instelling regio = se.getBeoordelingsEenheid().getParent().getRegio();
		or.add(Restrictions.eq("blokkade.regio.id", regio.getId()));

		if (standplaats != null)
		{
			or.add(Restrictions.eq("blokkade.standplaats.id", standplaats.getId()));
		}
		return criteria.list();
	}

}
