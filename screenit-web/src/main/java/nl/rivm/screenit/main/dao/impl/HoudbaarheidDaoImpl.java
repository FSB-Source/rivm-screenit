
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

import nl.rivm.screenit.main.dao.HoudbaarheidDao;
import nl.rivm.screenit.model.AbstractHoudbaarheid;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Strings;
import com.google.common.primitives.Ints;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class HoudbaarheidDaoImpl extends AbstractAutowiredDao implements HoudbaarheidDao
{

	@Override
	public <H extends AbstractHoudbaarheid> List<H> getHoudbaarheidItems(Class<H> clazz, long first, long count, String sortProperty, boolean asc)
	{
		Criteria crit = getSession().createCriteria(clazz);

		if (first > -1)
		{
			crit.setFirstResult(Ints.checkedCast(first));
		}

		if (count > -1)
		{
			crit.setMaxResults(Ints.checkedCast(count));
		}

		if (!Strings.isNullOrEmpty(sortProperty))
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

		return crit.list();
	}

	@Override
	public <H extends AbstractHoudbaarheid> Long countHoudbaarheidItems(Class<H> clazz)
	{
		Criteria crit = this.getSession().createCriteria(clazz);

		crit.setProjection(Projections.rowCount());
		Object uniqueResult = crit.uniqueResult();
		if (uniqueResult == null)
		{
			return 0L;
		}
		return (Long) uniqueResult;
	}

	@Override
	public <H extends AbstractHoudbaarheid> List<H> overlaptBestaandeReeks(H houdbaarheid)
	{
		Criteria crit = this.getSession().createCriteria(HibernateHelper.deproxy(houdbaarheid).getClass());
		String barcodeStart = houdbaarheid.getBarcodeStart();
		String barcodeEind = houdbaarheid.getBarcodeEnd();

		Integer length = barcodeStart.length();

		Criterion barcodeStartRestriction = Restrictions.and(Restrictions.le("barcodeStart", barcodeStart), Restrictions.ge("barcodeEnd", barcodeStart));
		Criterion barcodeEindRestriction = Restrictions.and(Restrictions.le("barcodeStart", barcodeEind), Restrictions.ge("barcodeEnd", barcodeEind));
		Criterion barcodeTotaalOverlapRestriction = Restrictions.and(Restrictions.gt("barcodeStart", barcodeStart), Restrictions.lt("barcodeEnd", barcodeEind));
		crit.add(Restrictions.or(barcodeStartRestriction, barcodeEindRestriction, barcodeTotaalOverlapRestriction));
		crit.add(Restrictions.eq("lengthBarcode", length));
		if (houdbaarheid.getId() != null)
		{
			crit.add(Restrictions.ne("id", houdbaarheid.getId()));
		}
		return crit.list();
	}
}
