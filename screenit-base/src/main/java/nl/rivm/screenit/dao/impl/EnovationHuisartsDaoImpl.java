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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.EnovationHuisartsDao;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class EnovationHuisartsDaoImpl extends AbstractAutowiredDao implements EnovationHuisartsDao
{

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdate(EnovationHuisarts huisarts)
	{
		getSession().saveOrUpdate(huisarts);
	}

	@Override
	public EnovationHuisarts getHuisartsByKlantnummer(String klantnummer)
	{
		Criteria crit = getSession().createCriteria(EnovationHuisarts.class);
		crit.add(Restrictions.eq("klantnummer", klantnummer));
		return (EnovationHuisarts) crit.uniqueResult();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<EnovationHuisarts> zoekHuisartsen(EnovationHuisarts zoekObject, String sortProperty, boolean ascending, int first, int count)
	{
		Criteria crit = maakCriteria(zoekObject);

		if (sortProperty != null)
		{
			if (ascending)
			{
				crit.addOrder(Order.asc(sortProperty));
			}
			else
			{
				crit.addOrder(Order.desc(sortProperty));
			}
		}
		crit.setFirstResult(first);
		crit.setMaxResults(count);

		return new ArrayList<EnovationHuisarts>(crit.list());
	}

	@Override
	public long telHuisartsen(EnovationHuisarts zoekObject)
	{
		Criteria crit = maakCriteria(zoekObject);
		crit.setProjection(Projections.countDistinct("id"));

		return ((Number) crit.uniqueResult()).longValue();
	}

	private Criteria maakCriteria(EnovationHuisarts zoekObject)
	{
		Criteria criteria = getSession().createCriteria(EnovationHuisarts.class);
		criteria.createAlias("adres", "adres");
		if (StringUtils.isNotBlank(zoekObject.getAchternaam()))
		{
			criteria.add(Restrictions.or(
				Restrictions.ilike("achternaam", zoekObject.getAchternaam(), MatchMode.ANYWHERE),
				Restrictions.ilike("weergavenaam", zoekObject.getAchternaam(), MatchMode.ANYWHERE),
				Restrictions.ilike("praktijknaam", zoekObject.getAchternaam(), MatchMode.ANYWHERE)));
		}
		Adres zoekAdres = zoekObject.getAdres();
		if (zoekAdres != null)
		{
			if (StringUtils.isNotBlank(zoekAdres.getPostcode()))
			{
				String postcode = StringUtils.deleteWhitespace(zoekAdres.getPostcode());

				criteria.add(Restrictions.ilike("adres.postcode", postcode, MatchMode.ANYWHERE));
			}
			if (StringUtils.isNotBlank(zoekAdres.getPlaats()))
			{
				criteria.add(Restrictions.ilike("adres.plaats", zoekAdres.getPlaats(), MatchMode.ANYWHERE));
			}
			if (StringUtils.isNotBlank(zoekAdres.getStraat()))
			{
				criteria.add(Restrictions.ilike("adres.straat", zoekAdres.getStraat(), MatchMode.ANYWHERE));
			}
		}
		criteria.add(Restrictions.eq("verwijderd", false));
		return criteria;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public void verwijderdeHuisartsenOntkoppelen()
	{
		ontkoppelDKHuisartsen();
	}

	private void ontkoppelDKHuisartsen()
	{
		Criteria criteria = getSession().createCriteria(ColonScreeningRonde.class);
		criteria.createAlias("colonHuisarts", "colonHuisarts");
		criteria.add(Restrictions.eq("colonHuisarts.verwijderd", true));
		List<ColonScreeningRonde> lijst = criteria.list();
		if (CollectionUtils.isNotEmpty(lijst))
		{
			for (ColonScreeningRonde ronde : lijst)
			{
				ronde.setColonHuisarts(null);
				getSession().saveOrUpdate(ronde);
			}
		}
	}

	@Override
	public EnovationHuisarts getHuisartsByAgb(String agbCode)
	{
		Criteria crit = getSession().createCriteria(EnovationHuisarts.class);
		crit.add(Restrictions.eq("huisartsAgb", agbCode));
		crit.add(Restrictions.eq("verwijderd", false));
		List list = crit.list();
		EnovationHuisarts huisarts = null;
		if (list != null && list.size() == 1)
		{
			huisarts = (EnovationHuisarts) list.get(0);
		}
		return huisarts;
	}

	@Override
	public List<String> getKlantnummersVanAlleActiveHuisartens()
	{
		Criteria crit = getSession().createCriteria(EnovationHuisarts.class);
		crit.add(Restrictions.eq("verwijderd", false));
		crit.setProjection(Projections.property("klantnummer"));
		return crit.list();
	}

	@Override
	public void verwijderHuisartsen(List<String> activeKlantnummers)
	{
		Criteria criteria = getSession().createCriteria(EnovationHuisarts.class);
		criteria.add(Restrictions.eq("verwijderd", false));
		criteria.add(Restrictions.in("klantnummer", activeKlantnummers));
		List<EnovationHuisarts> lijst = criteria.list();
		if (CollectionUtils.isNotEmpty(lijst))
		{
			Date vandaag = dateSupplier.getDate();
			for (EnovationHuisarts huisarts : lijst)
			{
				huisarts.setVerwijderd(true);
				huisarts.setGewijzigd(vandaag);
				getSession().saveOrUpdate(huisarts);
			}
		}
	}

}
