package nl.rivm.screenit.main.dao.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.dao.mamma.MammaPostcodeReeksDao;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang.math.NumberUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaPostcodeReeksDaoImpl extends AbstractAutowiredDao implements MammaPostcodeReeksDao
{

	@Override
	@Transactional(readOnly = true)
	public List<MammaPostcodeReeks> zoekPostcodeReeksen(MammaPostcodeReeks zoekObject, int first, int count, String sortProperty, boolean asc)
	{
		Criteria crit = createCriteria(zoekObject);
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

	private Criteria createCriteria(MammaPostcodeReeks zoekObject)
	{
		Criteria criteria = getSession().createCriteria(MammaPostcodeReeks.class);

		criteria.createAlias("standplaats", "standplaats");
		criteria.createAlias("standplaats.regio", "regio");
		if (zoekObject.getStandplaats() != null)
		{
			if (zoekObject.getStandplaats().getId() != null)
			{
				criteria.add(Restrictions.eq("standplaats", zoekObject.getStandplaats()));
			}
			else if (zoekObject.getStandplaats().getRegio() != null)
			{
				criteria.add(Restrictions.eq("standplaats.regio", zoekObject.getStandplaats().getRegio()));
			}
		}
		if (zoekObject.getVanPostcode() != null)
		{
			String vanPostcode = zoekObject.getVanPostcode().toUpperCase();
			String totPostcode = vanPostcode;
			if (vanPostcode.length() == 4 && NumberUtils.isNumber(vanPostcode))
			{
				vanPostcode += "ZZ";
				totPostcode += "AA";
			}
			criteria.add(Restrictions.le("vanPostcode", vanPostcode));
			criteria.add(Restrictions.ge("totPostcode", totPostcode));
		}
		return criteria;
	}

	@Override
	@Transactional(readOnly = true)
	public long countPostcodeReeksen(MammaPostcodeReeks zoekObject)
	{
		Criteria crit = createCriteria(zoekObject);
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	@Override
	@Transactional(readOnly = true)
	public List<MammaPostcodeReeks> overlaptBestaandeReeks(MammaPostcodeReeks postcodeReeks)
	{
		Criteria crit = this.getSession().createCriteria(MammaPostcodeReeks.class);
		String vanPostcode = postcodeReeks.getVanPostcode().toUpperCase();
		String totPostcode = postcodeReeks.getTotPostcode().toUpperCase();

		Criterion vanPostcodeRestriction = Restrictions.and(Restrictions.le("vanPostcode", vanPostcode), Restrictions.ge("totPostcode", vanPostcode));
		Criterion totPoscodeRestriction = Restrictions.and(Restrictions.le("vanPostcode", totPostcode), Restrictions.ge("totPostcode", totPostcode));
		Criterion barcodeTotaalOverlapRestriction = Restrictions.and(Restrictions.gt("vanPostcode", vanPostcode), Restrictions.lt("totPostcode", totPostcode));
		crit.add(Restrictions.or(vanPostcodeRestriction, totPoscodeRestriction, barcodeTotaalOverlapRestriction));
		if (postcodeReeks.getId() != null)
		{
			crit.add(Restrictions.ne("id", postcodeReeks.getId()));
		}

		return crit.list();
	}

	@Override
	public List<ScreeningOrganisatie> reeksInRegios(MammaPostcodeReeks postcodeReeks)
	{
		Criteria crit = this.getSession().createCriteria(BagAdres.class);
		crit.createAlias("gbaGemeente", "gemeente");

		String vanPostcode = postcodeReeks.getVanPostcode().toUpperCase();
		String totPostcode = postcodeReeks.getTotPostcode().toUpperCase();

		crit.add(Restrictions.ge("postcode", vanPostcode));
		crit.add(Restrictions.le("postcode", totPostcode));

		crit.setProjection(Projections.distinct(Projections.property("gemeente.screeningOrganisatie")));
		return crit.list();
	}

}
