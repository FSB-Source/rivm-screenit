package nl.rivm.screenit.dao.mamma.impl;

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

import java.util.List;

import nl.rivm.screenit.dao.mamma.MammaIMSDao;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.mamma.berichten.MammaIMSBericht;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.REQUIRED)
public class MammaIMSDaoImpl extends AbstractAutowiredDao implements MammaIMSDao
{

	@Override
	public boolean isBerichtAlOntvangen(String messageId)
	{
		Criteria criteria = getSession().createCriteria(MammaIMSBericht.class);
		criteria.add(Restrictions.eq("messageId", messageId));
		return criteria.list().size() >= 1;
	}

	@Override
	public List<MammaIMSBericht> getAlleNietVerwerkteIMSBerichten()
	{
		Criteria criteria = getSession().createCriteria(MammaIMSBericht.class);
		criteria.add(Restrictions.eq("berichtStatus", BerichtStatus.NIEUW));
		criteria.addOrder(Order.asc("id"));
		return criteria.list();
	}
}
