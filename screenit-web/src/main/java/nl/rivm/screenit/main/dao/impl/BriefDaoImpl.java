package nl.rivm.screenit.main.dao.impl;

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

import java.util.List;

import nl.rivm.screenit.main.dao.BriefDao;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.MergedBrievenFilter;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Repository;

import com.google.common.primitives.Ints;

@Repository
public class BriefDaoImpl extends AbstractAutowiredDao implements BriefDao
{

	@Override
	public <MB extends MergedBrieven<?>> Long countMergedBrieven(ScreeningOrganisatie screeningOrganisatie, MergedBrievenFilter<MB> filter)
	{
		Criteria crit = getMergedBrievenCriteria(screeningOrganisatie, filter);
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue();
	}

	@SuppressWarnings("unchecked")
	@Override
	public <MB extends MergedBrieven<?>> List<MB> getMergedBrieven(ScreeningOrganisatie screeningOrganisatie, MergedBrievenFilter<MB> filter, long first, long count,
		String sortProperty, boolean ascending)
	{
		Criteria crit = getMergedBrievenCriteria(screeningOrganisatie, filter);
		crit.setFirstResult(Ints.checkedCast(first));
		crit.setMaxResults(Ints.checkedCast(count));
		if (sortProperty.startsWith("afgedruktDoor"))
		{
			crit.createAlias("afgedruktDoor", "afgedruktDoor", JoinType.LEFT_OUTER_JOIN);
		}
		if (ascending)
		{
			crit.addOrder(Order.asc(sortProperty));
		}
		else
		{
			crit.addOrder(Order.desc(sortProperty));
		}
		if (!"mergedBrieven.naam".equals(sortProperty))
		{
			crit.createAlias("mergedBrieven", "mergedBrieven");
			crit.addOrder(Order.asc("mergedBrieven.naam"));
		}
		return crit.list();
	}

	protected <MB extends MergedBrieven<?>> Criteria getMergedBrievenCriteria(ScreeningOrganisatie screeningOrganisatie, MergedBrievenFilter<MB> filter)
	{
		Criteria crit = this.getSession().createCriteria(filter.getMergedBrievenClass());
		if (screeningOrganisatie != null)
		{
			crit.add(Restrictions.eq("screeningOrganisatie", screeningOrganisatie));
		}
		if (filter.getActief() != null)
		{
			crit.add(Restrictions.eq("geprint", filter.getActief()));
		}
		if (filter.getControle() != null)
		{
			crit.add(Restrictions.eq("controle", filter.getControle()));
		}
		crit.add(Restrictions.eq("verwijderd", Boolean.FALSE));
		crit.add(Restrictions.eq("vrijgegeven", Boolean.TRUE));
		return crit;
	}

	@Override
	public List<? extends ClientBrief> getBrievenVanAfmelding(Afmelding afmelding, BriefType... types)
	{
		Criteria crit = null;
		Bevolkingsonderzoek onderzoek = afmelding.getBevolkingsonderzoek();
		if (Bevolkingsonderzoek.COLON.equals(onderzoek))
		{
			crit = getSession().createCriteria(ColonBrief.class);
		}
		else if (Bevolkingsonderzoek.CERVIX.equals(onderzoek))
		{
			crit = getSession().createCriteria(CervixBrief.class);
		}
		else if (Bevolkingsonderzoek.MAMMA.equals(onderzoek))
		{
			crit = getSession().createCriteria(MammaBrief.class);
		}
		crit.add(Restrictions.eq("afmelding", afmelding));
		crit.add(Restrictions.in("briefType", types));

		return crit.list();
	}

	@Override
	public List<BezwaarBrief> getBrievenVanBezwaar(BezwaarMoment moment)
	{
		Criteria crit = getSession().createCriteria(BezwaarBrief.class);
		crit.add(Restrictions.eq("bezwaarMoment", moment));
		return crit.list();
	}

}
