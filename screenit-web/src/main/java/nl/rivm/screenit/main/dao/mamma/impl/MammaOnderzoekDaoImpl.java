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

import java.util.List;

import nl.rivm.screenit.main.dao.mamma.MammaOnderzoekDao;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.util.query.ScreenitRestrictions;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.shiro.util.CollectionUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaOnderzoekDaoImpl extends AbstractAutowiredDao implements MammaOnderzoekDao
{

	@Override
	public List<MammaOnderzoek> zoekOnderbrokenOnderzoeken(MammaCeWerklijstZoekObject zoekObject, int first, int count, String sortProperty, boolean ascending)
	{
		Criteria criteria = createOnderbrokenOnderzoekenCriteria(zoekObject);
		return zoekOnderzoeken(first, count, sortProperty, ascending, criteria);
	}

	private List<MammaOnderzoek> zoekOnderzoeken(int first, int count, String sortProperty, boolean ascending, Criteria criteria)
	{
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
		criteria.setFirstResult(Math.max(first, 0));
		if (count > 0)
		{
			criteria.setMaxResults(count);
		}
		return criteria.list();
	}

	@Override
	public long countOnderbrokenOnderzoeken(MammaCeWerklijstZoekObject zoekObject)
	{
		Criteria criteria = createOnderbrokenOnderzoekenCriteria(zoekObject);
		criteria.setProjection(Projections.rowCount());
		return (Long) criteria.uniqueResult();
	}

	private Criteria createOnderbrokenOnderzoekenCriteria(MammaCeWerklijstZoekObject zoekObject)
	{
		Criteria criteria = createGenericCriteria(zoekObject);
		criteria.add(Restrictions.eq("status", MammaOnderzoekStatus.ONDERBROKEN));
		if (zoekObject.getMetBriefOproepOnderbrokenOnderzoek() != null)
		{
			criteria.createAlias("ronde.laatsteBrief", "brief", JoinType.LEFT_OUTER_JOIN);
			if (zoekObject.getMetBriefOproepOnderbrokenOnderzoek())
			{
				criteria.add(Restrictions.eq("brief.briefType", BriefType.MAMMA_OPROEP_OPNEMEN_CONTACT));
			}
			else
			{
				criteria.add(Restrictions.or(Restrictions.isNull("brief.briefType"), Restrictions.ne("brief.briefType", BriefType.MAMMA_OPROEP_OPNEMEN_CONTACT)));
			}
		}
		return criteria;
	}

	private Criteria createGenericCriteria(MammaCeWerklijstZoekObject zoekObject)
	{
		Criteria criteria = getSession().createCriteria(MammaOnderzoek.class);

		criteria.createAlias("screeningsEenheid", "se");
		criteria.createAlias("se.beoordelingsEenheid", "be");
		criteria.createAlias("be.parent", "ce");
		criteria.createAlias("afspraak", "afspraak");
		criteria.createAlias("afspraak.uitnodiging", "uitnodiging");
		criteria.createAlias("uitnodiging.screeningRonde", "ronde");
		criteria.createAlias("ronde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("mammografie", "mammografie");

		ScreenitRestrictions.addPersoonBaseRestrictions(criteria, "persoon.");

		criteria.add(Restrictions.eqProperty("dossier.laatsteScreeningRonde.id", "ronde.id"));
		criteria.add(Restrictions.eqProperty("ronde.laatsteUitnodiging.id", "uitnodiging.id"));
		criteria.add(Restrictions.eqProperty("uitnodiging.laatsteAfspraak.id", "afspraak.id"));
		criteria.add(Restrictions.eq("isDoorgevoerd", true));
		criteria.add(Restrictions.eq("ronde.status", ScreeningRondeStatus.LOPEND));

		if (!CollectionUtils.isEmpty(zoekObject.getScreeningsEenheden()))
		{
			criteria.add(Restrictions.in("screeningsEenheid", zoekObject.getScreeningsEenheden()));
		}
		else
		{
			criteria.add(Restrictions.eq("status", MammaOnderzoekStatus.ACTIEF)); 
		}
		return criteria;
	}
}
