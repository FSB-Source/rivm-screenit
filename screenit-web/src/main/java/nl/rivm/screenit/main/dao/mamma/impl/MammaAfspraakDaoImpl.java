package nl.rivm.screenit.main.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.dao.mamma.MammaAfspraakDao;
import nl.rivm.screenit.main.transformer.AfspraakDatumResultTransformer;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.type.StandardBasicTypes;
import org.hibernate.type.Type;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaAfspraakDaoImpl extends AbstractAutowiredDao implements MammaAfspraakDao
{
	@Override
	public List<AfspraakDatumResultTransformer.AfspraakDatum> getAfspraakDatums(MammaBlokkade blokkade)
	{
		Criteria criteria = getSession().createCriteria(MammaAfspraak.class, "afspraak");
		criteria.createAlias("afspraak.standplaatsPeriode", "standplaatsPeriode");
		criteria.createAlias("standplaatsPeriode.screeningsEenheid", "screeningsEenheid");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");

		criteria.add(Restrictions.ge("afspraak.vanaf", blokkade.getVanaf()));
		criteria.add(Restrictions.lt("afspraak.vanaf", DateUtil.toUtilDate(DateUtil.toLocalDate(blokkade.getTotEnMet()).plusDays(1))));
		criteria.add(Restrictions.in("afspraak.status", MammaAfspraakStatus.NIET_GEANNULEERD));
		criteria.add(Restrictions.eqProperty("uitnodiging.laatsteAfspraak", "afspraak.id"));

		switch (blokkade.getType())
		{
		case STANDPLAATS:
			criteria.createAlias("standplaatsPeriode.standplaatsRonde", "standplaatsRonde");
			criteria.createAlias("standplaatsRonde.standplaats", "standplaats");
			criteria.add(Restrictions.eq("standplaats.id", blokkade.getStandplaats().getId()));
			break;
		case SCREENINGS_ORGANISATIE:
			criteria.createAlias("screeningsEenheid.beoordelingsEenheid", "beoordelingsEenheid");
			criteria.createAlias("beoordelingsEenheid.parent", "centraleEenheid");
			criteria.createAlias("centraleEenheid.regio", "regio");
			criteria.add(Restrictions.eq("regio.id", blokkade.getRegio().getId()));
			break;
		case SCREENINGS_EENHEID:
			criteria.add(Restrictions.eq("screeningsEenheid.id", blokkade.getScreeningsEenheid().getId()));
			break;
		}

		criteria.addOrder(Order.asc("day"));

		criteria.setProjection(Projections.distinct(Projections.projectionList()
			.add(Projections.property("standplaatsPeriode.screeningsEenheid"))
			.add(Projections.alias(Projections.sqlProjection("date({alias}.vanaf) as day", new String[] { "day" }, new Type[] { StandardBasicTypes.DATE }), "day"))));

		return criteria.setResultTransformer(new AfspraakDatumResultTransformer()).list();
	}

	@Override
	public Date getDatumEersteGeplandeAfspraak(Long standplaatsPeriodeId)
	{
		Criteria criteria = getDatumGeplandeAfsprakenCriteria(standplaatsPeriodeId);
		criteria.setProjection(Projections.min("afspraak.vanaf"));
		criteria.setFirstResult(0);
		criteria.setMaxResults(1);

		return (Date) criteria.uniqueResult();
	}

	@Override
	public Date getDatumLaatsteGeplandeAfspraak(Long standplaatsPeriodeId)
	{
		Criteria criteria = getDatumGeplandeAfsprakenCriteria(standplaatsPeriodeId);
		criteria.setProjection(Projections.max("afspraak.vanaf"));
		criteria.setFirstResult(0);
		criteria.setMaxResults(1);

		return (Date) criteria.uniqueResult();
	}

	private Criteria getDatumGeplandeAfsprakenCriteria(Long standplaatsPeriodeId)
	{
		Criteria criteria = getSession().createCriteria(MammaUitnodiging.class, "uitnodiging");
		criteria.createAlias("uitnodiging.laatsteAfspraak", "afspraak");
		criteria.add(Restrictions.eq("afspraak.standplaatsPeriode.id", standplaatsPeriodeId));
		criteria.add(Restrictions.eq("afspraak.status", MammaAfspraakStatus.GEPLAND));

		return criteria;
	}
}
