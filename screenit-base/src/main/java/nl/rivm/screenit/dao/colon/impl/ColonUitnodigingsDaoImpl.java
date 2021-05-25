package nl.rivm.screenit.dao.colon.impl;

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

import java.util.List;

import nl.rivm.screenit.dao.colon.ColonUitnodigingsDao;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.UitnodigingCohort;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class ColonUitnodigingsDaoImpl extends AbstractAutowiredDao implements ColonUitnodigingsDao
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<Long> getTeVersturenUitnodigingen()
	{
		Criteria criteria = getSession().createCriteria(ColonUitnodiging.class);
		criteria.createAlias("screeningRonde", "screeningRondeUitnodiging");
		criteria.createAlias("screeningRondeUitnodiging.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");

		ScreenitRestrictions.addClientBaseRestrictions(criteria, "client", "persoon");

		criteria.add(Restrictions.eq("verstuurd", false));
		criteria.add(Restrictions.le("uitnodigingsDatum", currentDateSupplier.getDate()));

		DetachedCriteria subquery = DetachedCriteria.forClass(ColonBrief.class, "brief");
		subquery.setProjection(Projections.id());
		subquery.add(Restrictions.eqProperty("brief.screeningRonde", "screeningRondeUitnodiging.id"));
		subquery.add(Restrictions.or(Restrictions.eq("brief.briefType", BriefType.COLON_UITNODIGING_INTAKE), Restrictions.eq("brief.briefType", BriefType.COLON_GUNSTIGE_UITSLAG)));

		criteria.add(Subqueries.notExists(subquery));
		criteria.setProjection(Projections.id());
		return criteria.list();
	}

	@Override
	public List<Integer> getUitnodigingCohorten()
	{
		Criteria crit = this.getSession().createCriteria(UitnodigingCohort.class);
		crit.add(Restrictions.le("jaar", currentDateSupplier.getLocalDate().getYear()));
		crit.addOrder(Order.asc("jaar"));
		crit.setProjection(Projections.property("jaar"));
		return crit.list();
	}

	@Override
	public UitnodigingCohort getUitnodigingCohort(Integer jaar)
	{
		Criteria crit = this.getSession().createCriteria(UitnodigingCohort.class);
		crit.add(Restrictions.eq("jaar", jaar));
		return (UitnodigingCohort) crit.uniqueResult();
	}

}
