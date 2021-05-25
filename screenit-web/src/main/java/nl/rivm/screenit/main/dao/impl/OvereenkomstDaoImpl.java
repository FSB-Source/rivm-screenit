
package nl.rivm.screenit.main.dao.impl;

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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.dao.OvereenkomstDao;
import nl.rivm.screenit.main.web.gebruiker.screening.colon.overeenkomstenzoeken.OvereenkomstZoekFilter;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenInstellingOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.OvereenkomstType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.query.DateYearRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.hibernate.restrictions.NvlRestrictions;

import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.hibernate.sql.JoinType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class OvereenkomstDaoImpl extends AbstractAutowiredDao implements OvereenkomstDao
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<Overeenkomst> getOvereenkomsten(Boolean actief, Long first, Long size, String sortProperty, boolean asc)
	{
		Criteria criteria = this.getSession().createCriteria(Overeenkomst.class);
		criteria.createAlias("document", "document");

		if (actief != null)
		{
			criteria.add(Restrictions.eq("actief", actief));
		}

		criteria.setFirstResult(first.intValue());
		criteria.setMaxResults(size.intValue());

		if (asc)
		{
			criteria.addOrder(Order.asc(sortProperty));
		}
		else
		{
			criteria.addOrder(Order.desc(sortProperty));
		}

		return criteria.list();
	}

	@Override
	public <T> List<AbstractAfgeslotenOvereenkomst> getAfgeslotenOvereenkomsten(Class<? extends AbstractAfgeslotenOvereenkomst> returnType, T filter, Boolean actief, Long first,
		Long size, String sortProperty, boolean asc)
	{
		Criteria criteria = this.getSession().createCriteria(returnType);
		criteria.createAlias("overeenkomst", "overeenkomst");
		criteria.createAlias("screeningOrganisatie", "screeningOrganisatie");

		addFilter(returnType, filter, criteria);

		getAfgeslotenOvereenkomstActiefCriteria(actief, criteria);

		criteria.setFirstResult(first.intValue());
		criteria.setMaxResults(size.intValue());

		if (asc)
		{
			criteria.addOrder(Order.asc(sortProperty));
		}
		else
		{
			criteria.addOrder(Order.desc(sortProperty));
		}

		return criteria.list();
	}

	protected <T> void addFilter(Class<? extends AbstractAfgeslotenOvereenkomst> returnType, T filter, Criteria criteria)
	{
		if (returnType.equals(AfgeslotenInstellingOvereenkomst.class))
		{
			criteria.add(Restrictions.eq("instelling", filter));
		}
		else if (returnType.equals(AfgeslotenMedewerkerOvereenkomst.class))
		{
			criteria.add(Restrictions.eq("gebruiker", filter));
		}
	}

	protected void getAfgeslotenOvereenkomstActiefCriteria(Boolean actief, Criteria criteria)
	{
		if (BooleanUtils.isTrue(actief))
		{
			criteria.add(NvlRestrictions.le("startDatum", currentDateSupplier.getDate(), Constants.BEGIN_OF_TIME));
			criteria.add(NvlRestrictions.ge("eindDatum", currentDateSupplier.getDate(), Constants.END_OF_TIME));
		}
		else if (BooleanUtils.isFalse(actief))
		{
			criteria.add(Restrictions.or(NvlRestrictions.gt("startDatum", currentDateSupplier.getDate(), Constants.BEGIN_OF_TIME),
				NvlRestrictions.lt("eindDatum", currentDateSupplier.getDate(), Constants.END_OF_TIME)));
		}
	}

	@Override
	public <T> long countAfgeslotenOvereenkomsten(Class<? extends AbstractAfgeslotenOvereenkomst> returnType, T filter, Boolean actief)
	{
		Criteria criteria = this.getSession().createCriteria(returnType);
		addFilter(returnType, filter, criteria);
		getAfgeslotenOvereenkomstActiefCriteria(actief, criteria);
		criteria.setProjection(Projections.rowCount());
		return ((Number) criteria.uniqueResult()).longValue();
	}

	@Override
	public long countOvereenkomsten(Boolean actief)
	{
		Criteria criteria = this.getSession().createCriteria(Overeenkomst.class);
		if (actief != null)
		{
			criteria.add(Restrictions.eq("actief", actief));
		}
		criteria.setProjection(Projections.rowCount());
		return ((Number) criteria.uniqueResult()).longValue();
	}

	@Override
	public List<Overeenkomst> getOvereenkomsten(OrganisatieType organisatieType, OvereenkomstType... overeenkomstTypes)
	{
		Criteria criteria = this.getSession().createCriteria(Overeenkomst.class);
		criteria.add(Restrictions.eq("actief", Boolean.TRUE));
		criteria.add(Restrictions.in("overeenkomst", overeenkomstTypes));

		if (organisatieType != null)
		{
			criteria.add(Restrictions.eq("organisatieType", organisatieType));
		}

		return criteria.list();
	}

	@Override
	public int getVolgnummerOvereenkomst()
	{
		Criteria criteria = this.getSession().createCriteria(AbstractAfgeslotenOvereenkomst.class);
		criteria.add(DateYearRestrictions.eq("startDatum", currentDateSupplier.getDateTime().getYear()));
		criteria.setProjection(Projections.rowCount());

		return ((Number) criteria.uniqueResult()).intValue() + 1;
	}

	@Override
	public List<AbstractAfgeslotenOvereenkomst> getTeAccoderenOvereenkomsten(InstellingGebruiker inTeLoggenInstellingGebruiker)
	{
		Criteria criteria = getCriteriaTeAccoderen(inTeLoggenInstellingGebruiker);
		criteria.addOrder(Order.asc("startDatum"));
		return criteria.list();
	}

	@Override
	public long countTeAccoderenOvereenkomsten(InstellingGebruiker inTeLoggenInstellingGebruiker)
	{
		Criteria criteria = getCriteriaTeAccoderen(inTeLoggenInstellingGebruiker);

		criteria.setProjection(Projections.rowCount());

		return ((Number) criteria.uniqueResult()).longValue();
	}

	protected Criteria getCriteriaTeAccoderen(InstellingGebruiker inTeLoggenInstellingGebruiker)
	{
		Criteria criteria = this.getSession().createCriteria(AbstractAfgeslotenOvereenkomst.class);
		criteria.createAlias("gebruiker", "gebruiker", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("instelling", "instelling", JoinType.LEFT_OUTER_JOIN);

		criteria.add(Restrictions.isNull("akkoordDatum"));
		criteria.add(Restrictions.eq("teAccoderen", Boolean.TRUE));

		criteria.add(NvlRestrictions.ge("eindDatum", currentDateSupplier.getDate(), Constants.END_OF_TIME));

		Disjunction disjunction = Restrictions.disjunction();
		disjunction.add(Restrictions.eq("gebruiker", inTeLoggenInstellingGebruiker.getMedewerker()));
		disjunction.add(Restrictions.and(Restrictions.eq("instelling", inTeLoggenInstellingGebruiker.getOrganisatie()),
			Restrictions.eq("instelling.gemachtigde", inTeLoggenInstellingGebruiker.getMedewerker())));

		criteria.add(disjunction);
		return criteria;
	}

	@Override
	public List<Instelling> getAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter, String sortProperty, boolean ascending, int first, int count)
	{
		Criteria criteria = this.getSession().createCriteria(Instelling.class);
		maakOvereenkomstZoekenCrit(filter, criteria);
		if (sortProperty != null)
		{
			if (ascending)
			{
				criteria.addOrder(Order.asc(sortProperty));
			}
			else
			{
				criteria.addOrder(Order.desc(sortProperty));
			}
		}
		criteria.setFirstResult(first);
		criteria.setMaxResults(count);
		return criteria.list();
	}

	@Override
	public long countAfgeslotenOvereenkomsten(OvereenkomstZoekFilter filter)
	{
		Criteria criteria = this.getSession().createCriteria(Instelling.class);
		maakOvereenkomstZoekenCrit(filter, criteria);
		ProjectionList projectionList = Projections.projectionList() 
			.add(Projections.count("id"));
		criteria.setProjection(projectionList);
		return (Long) criteria.uniqueResult();
	}

	private void maakOvereenkomstZoekenCrit(OvereenkomstZoekFilter filter, Criteria criteria)
	{
		DetachedCriteria subqueryOvereenkomsten = DetachedCriteria.forClass(AfgeslotenInstellingOvereenkomst.class);
		subqueryOvereenkomsten.createAlias("instelling", "instelling");
		subqueryOvereenkomsten.setProjection(Projections.property("instelling.id"));

		if (StringUtils.isNotBlank(filter.getOrganisatieNaam()))
		{
			criteria.add(Restrictions.ilike("naam", filter.getOrganisatieNaam(), MatchMode.ANYWHERE));
		}
		if (filter.getOrganisatieType() != null)
		{
			criteria.add(Restrictions.eq("organisatieType", filter.getOrganisatieType()));
		}
		if (StringUtils.isNotBlank(filter.getOrganisatieUra()))
		{
			criteria.add(Restrictions.eq("uziAbonneenummer", filter.getOrganisatieUra()));
		}
		if (filter.getOvereenkomst() != null)
		{
			subqueryOvereenkomsten.add(Restrictions.eq("overeenkomst", filter.getOvereenkomst()));
		}
		if (filter.getRegio() != null)
		{
			criteria.createAlias("parent", "instparent", JoinType.LEFT_OUTER_JOIN);
			criteria.add(Restrictions.or(Restrictions.eq("parent", filter.getRegio()), Restrictions.eq("instparent.parent", filter.getRegio())));
		}
		if (filter.getLopendeDatum() != null)
		{
			subqueryOvereenkomsten.add(Restrictions.le("startDatum", filter.getLopendeDatum()));
			subqueryOvereenkomsten.add(Restrictions.or(Restrictions.ge("eindDatum", filter.getLopendeDatum()), Restrictions.isNull("eindDatum")));
		}

		DetachedCriteria subquery = DetachedCriteria.forClass(Instelling.class);
		subquery.createAlias("adressen", "adressen", JoinType.LEFT_OUTER_JOIN);
		boolean addressenSubQuery = false;
		if (StringUtils.isNotBlank(filter.getOrganisatiePlaats()))
		{
			subquery.add(Restrictions.ilike("adressen.plaats", filter.getOrganisatiePlaats(), MatchMode.ANYWHERE));
			addressenSubQuery = true;
		}
		if (StringUtils.isNotBlank(filter.getOrganisatiePostcode()))
		{
			subquery.add(Restrictions.ilike("adressen.postcode", filter.getOrganisatiePostcode(), MatchMode.ANYWHERE));
			addressenSubQuery = true;
		}
		if (addressenSubQuery)
		{
			subquery.setProjection(Projections.property("id"));
			criteria.add(Subqueries.propertyIn("id", subquery));
		}

		criteria.add(Subqueries.propertyIn("id", subqueryOvereenkomsten));
	}

	@Override
	public List<AfgeslotenInstellingOvereenkomst> getAfgeslotenOvereenkomstenBijInstelling(OvereenkomstZoekFilter filter, Instelling instelling)
	{
		Criteria criteria = this.getSession().createCriteria(AfgeslotenInstellingOvereenkomst.class);
		criteria.add(Restrictions.eq("instelling", instelling));
		if (filter.getLopendeDatum() != null)
		{
			criteria.add(Restrictions.le("startDatum", filter.getLopendeDatum()));
			criteria.add(Restrictions.or(Restrictions.ge("eindDatum", filter.getLopendeDatum()), Restrictions.isNull("eindDatum")));
		}
		return criteria.list();
	}

	@Override
	public List<Overeenkomst> getAlleOvereenkomstenVanTypeOvereenkomst()
	{
		Criteria criteria = this.getSession().createCriteria(Overeenkomst.class);
		criteria.add(Restrictions.eq("overeenkomst", OvereenkomstType.OVEREENKOMST));
		return criteria.list();
	}

	@Override
	public boolean isErAlEenZakelijkOvereenkomst(Overeenkomst overeenkomst)
	{
		Criteria crit = getSession().createCriteria(Overeenkomst.class);
		crit.add(Restrictions.eq("overeenkomst", OvereenkomstType.ZAKELIJKE_OVEREENKOMST));
		if (overeenkomst != null && overeenkomst.getId() != null)
		{
			crit.add(Restrictions.ne("id", overeenkomst.getId()));
		}
		return crit.list().size() > 0;
	}
}
