
package nl.rivm.screenit.dao.impl;

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

import nl.rivm.screenit.dao.BaseHoudbaarheidDao;
import nl.rivm.screenit.model.AbstractHoudbaarheid;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

@Repository
public class BaseHoudbaarheidDaoImpl extends AbstractAutowiredDao implements BaseHoudbaarheidDao
{

	@Override
	public <H extends AbstractHoudbaarheid> H getHoudbaarheidVoor(Class<H> clazz, String barcode)
	{
		Criteria crit = getSession().createCriteria(clazz);
		crit.add(Restrictions.le("barcodeStart", barcode));
		crit.add(Restrictions.ge("barcodeEnd", barcode));
		crit.add(Restrictions.eq("lengthBarcode", barcode.length()));
		return (H) crit.uniqueResult();
	}

}
