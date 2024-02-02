package nl.rivm.screenit.main.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.dao.IfobtBestandDao;
import nl.rivm.screenit.main.model.colon.IFobtBatchFilter;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.criteria.ListCriteria;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

import com.google.common.primitives.Ints;

@Repository
public class IfobtBestandDaoImpl extends AbstractAutowiredDao implements IfobtBestandDao
{

	@Override
	public Iterator<? extends IFOBTBestand> getBestanden(IFobtBatchFilter filter, long first, long count, String sortProperty, boolean ascending)
	{
		BaseCriteria<IFOBTBestand> crit = createCriteria(filter);
		if (sortProperty.startsWith("laboratorium"))
		{
			crit.alias("laboratorium");
		}
		return crit.list(getSession(), new ListCriteria(Ints.checkedCast(first), Ints.checkedCast(count), sortProperty, ascending)).iterator();
	}

	private BaseCriteria<IFOBTBestand> createCriteria(IFobtBatchFilter filter)
	{
		BaseCriteria<IFOBTBestand> crit = new BaseCriteria<>(IFOBTBestand.class);
		List<IFOBTBestandStatus> statussen = new ArrayList<>();
		if (filter.getStatus() == null)
		{
			statussen = Arrays.asList(IFOBTBestandStatus.GEAUTORISEERD, IFOBTBestandStatus.INGELEZEN, IFOBTBestandStatus.VERWERKT);
		}
		else if (filter.getStatus().equals(IFOBTBestandStatus.VERWERKT))
		{
			statussen = Arrays.asList(IFOBTBestandStatus.GEAUTORISEERD, IFOBTBestandStatus.VERWERKT);
		}
		else if (filter.getStatus().equals(IFOBTBestandStatus.INGELEZEN))
		{
			statussen = List.of(IFOBTBestandStatus.INGELEZEN);
		}
		crit.add(Restrictions.in("status", statussen));
		if (filter.getLab() != null)
		{
			crit.add(Restrictions.eq("laboratorium", filter.getLab()));
		}
		if (filter.isAnalyseDatum())
		{
			DetachedCriteria subQuery = DetachedCriteria.forClass(IFOBTUitslag.class);
			if (filter.getDatumTot() != null)
			{
				subQuery.add(Restrictions.lt("analyseDatum", DateUtil.plusDagen(DateUtil.startDag(filter.getDatumTot()), 1)));
			}
			if (filter.getDatumVan() != null)
			{
				subQuery.add(Restrictions.ge("analyseDatum", DateUtil.startDag(filter.getDatumVan())));
			}
			subQuery.setProjection(Projections.property("bestand"));
			crit.in("id", subQuery);
		}
		else
		{
			if (filter.getDatumTot() != null)
			{
				crit.add(Restrictions.lt("statusDatum", DateUtil.plusDagen(DateUtil.startDag(filter.getDatumTot()), 1)));
			}
			if (filter.getDatumVan() != null)
			{
				crit.add(Restrictions.ge("statusDatum", DateUtil.startDag(filter.getDatumVan())));
			}
		}
		return crit;
	}

	@Override
	public long countBestanden(IFobtBatchFilter filter)
	{
		return createCriteria(filter).countLong(getSession(), "id");
	}

}
