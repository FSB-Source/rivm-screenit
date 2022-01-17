
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
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.GemeenteDao;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UitnodigingsGebied;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.restrictions.NvlRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.common.primitives.Ints;

@Repository
public class GemeenteDaoImpl extends AbstractAutowiredDao implements GemeenteDao
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<Gemeente> zoekGemeentes(Gemeente zoekObject, long first, long count, String property, boolean ascending)
	{
		BaseCriteria<Gemeente> criteria = createCriteria(zoekObject);

		return criteria.list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count), property, ascending));
	}

	private BaseCriteria<Gemeente> createCriteria(Gemeente zoekObject)
	{
		BaseCriteria<Gemeente> criteria = new BaseCriteria<Gemeente>(Gemeente.class);
		if (StringUtils.isNotBlank(zoekObject.getNaam()))
		{
			criteria.add(Restrictions.ilike("naam", zoekObject.getNaam(), MatchMode.ANYWHERE));
		}
		else
		{

			criteria.add(Restrictions.ilike("naam", "123456", MatchMode.ANYWHERE));
		}
		if (zoekObject.getScreeningOrganisatie() != null)
		{
			criteria.add(Restrictions.eq("screeningOrganisatie", zoekObject.getScreeningOrganisatie()));
		}

		return criteria;
	}

	@Override
	public long countGemeentes(Gemeente zoekObject)
	{
		return createCriteria(zoekObject).countLong(getSession());
	}

	@Override
	public List<String> getWoonplaatsen(UitnodigingsGebied gebied)
	{
		Criteria criteria = getSession().createCriteria(BagAdres.class);
		criteria.add(Restrictions.eq("gbaGemeente", gebied.getGemeente()));
		criteria.setProjection(Projections.distinct(Projections.property("plaats")));
		return criteria.list();
	}

	@Override
	public Iterator<? extends UitnodigingsGebied> getGebieden(UitnodigingsGebied zoekObject, ColoscopieCentrum coloscopieCentrum, long first, long count, String property,
		boolean ascending)
	{
		BaseCriteria<UitnodigingsGebied> criteria = createCriteria(zoekObject, coloscopieCentrum);

		return criteria.list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count), property, ascending)).iterator();
	}

	@Override
	public long getCountGebieden(UitnodigingsGebied zoekObject, ColoscopieCentrum coloscopieCentrum)
	{
		return createCriteria(zoekObject, coloscopieCentrum).countLong(getSession());
	}

	private BaseCriteria<UitnodigingsGebied> createCriteria(UitnodigingsGebied zoekObject, ColoscopieCentrum coloscopieCentrum)
	{
		BaseCriteria<UitnodigingsGebied> criteria = new BaseCriteria<UitnodigingsGebied>(UitnodigingsGebied.class);
		String zoekNaam = zoekObject.getNaam();
		if (StringUtils.isBlank(zoekNaam))
		{
			zoekNaam = "%";
		}
		criteria.add(Restrictions.ilike("naam", zoekNaam, MatchMode.ANYWHERE));
		criteria.createAlias("gemeente", "gemeente");
		criteria.add(Restrictions.isNotNull("gemeente.screeningOrganisatie"));

		DetachedCriteria subQuery = DetachedCriteria.forClass(ColoscopieCentrumColonCapaciteitVerdeling.class);
		subQuery.add(Restrictions.eq("coloscopieCentrum", coloscopieCentrum));
		subQuery.setProjection(Projections.distinct(Projections.property("uitnodigingsGebied.id")));

		criteria.add(Subqueries.propertyNotIn("id", subQuery));

		return criteria;
	}

	@Override
	public List<Gemeente> getGemeentesZonderScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		BaseCriteria<Gemeente> criteria = new BaseCriteria<Gemeente>(Gemeente.class);
		Date nu = currentDateSupplier.getDate();
		criteria.add(NvlRestrictions.le("beginDatum", nu, Constants.BEGIN_OF_TIME));
		criteria.add(NvlRestrictions.ge("eindDatum", nu, Constants.END_OF_TIME));
		criteria.add(Restrictions.or(Restrictions.isNull("screeningOrganisatie"), Restrictions.eq("screeningOrganisatie", screeningOrganisatie)));

		criteria.addOrder(Order.asc("naam"));

		return criteria.list(getSession());
	}

	@Override
	public List<Gemeente> getGemeentesZonderBMHKLaboratorium(BMHKLaboratorium bmhkLaboratorium)
	{
		BaseCriteria<Gemeente> criteria = new BaseCriteria<>(Gemeente.class);
		Date nu = currentDateSupplier.getDate();
		criteria.add(NvlRestrictions.le("beginDatum", nu, Constants.BEGIN_OF_TIME));
		criteria.add(NvlRestrictions.ge("eindDatum", nu, Constants.END_OF_TIME));
		criteria.add(Restrictions.or(Restrictions.isNull("bmhkLaboratorium"), Restrictions.eq("bmhkLaboratorium", bmhkLaboratorium)));

		criteria.addOrder(Order.asc("naam"));

		return criteria.list(getSession());
	}

	@Override
	public List<Gemeente> getAllGekoppeldeGemeentes()
	{
		BaseCriteria<Gemeente> criteria = new BaseCriteria<Gemeente>(Gemeente.class);
		Date nu = currentDateSupplier.getDate();
		criteria.add(NvlRestrictions.le("beginDatum", nu, Constants.BEGIN_OF_TIME));
		criteria.add(NvlRestrictions.ge("eindDatum", nu, Constants.END_OF_TIME));
		criteria.add(Restrictions.isNotNull("screeningOrganisatie"));

		criteria.addOrder(Order.asc("naam"));

		return criteria.list(getSession());
	}

}
