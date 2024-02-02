
package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.dao.KwaliteitscontroleDao;
import nl.rivm.screenit.model.colon.SKMLControleBarcode;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

@Repository
public class KwaliteitscontroleDaoImpl extends AbstractAutowiredDao implements KwaliteitscontroleDao
{

	@Override
	public SKMLControleBarcode getSkmlBarcode(String barcode)
	{
		Criteria crit = getSession().createCriteria(SKMLControleBarcode.class);
		crit.add(Restrictions.eq("barcode", barcode));
		return (SKMLControleBarcode) crit.uniqueResult();
	}

}
