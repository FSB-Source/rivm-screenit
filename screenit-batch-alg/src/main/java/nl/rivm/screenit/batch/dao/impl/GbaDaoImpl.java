
package nl.rivm.screenit.batch.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import nl.rivm.screenit.batch.dao.GbaDao;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.gba.GbaStamtabel;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class GbaDaoImpl extends AbstractAutowiredDao implements GbaDao
{
	
	@Override
	public <T extends GbaStamtabel> T getStamtabelByCode(Class<T> clazz, String code)
	{
		Criteria crit = this.getSession().createCriteria(clazz);
		crit.add(Restrictions.eq("code", code));

		return (T) crit.uniqueResult();
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void updateVerstuurdeVragen(String uniqueBatchId)
	{
		Query query = getSession().createQuery("update " + GbaVraag.class.getName() + " set verstuurd = true where uniqueBatchId = :batchid");

		query.setParameter("batchid", uniqueBatchId).executeUpdate();
	}

	@Override
	public GbaVraag getLaatsteGbaVraag(Client client, String bsn)
	{
		Criteria crit = getSession().createCriteria(GbaVraag.class);
		if (client != null)
		{
			crit.add(Restrictions.or(Restrictions.eq("bsn", client.getPersoon().getBsn()), Restrictions.eq("client", client)));
		}
		else
		{
			crit.add(Restrictions.eq("bsn", bsn));
		}

		crit.setMaxResults(1);

		crit.addOrder(Order.desc("id"));

		return (GbaVraag) crit.uniqueResult();
	}

	@Override
	public Criteria getAllAdressenZonderCoordinanten()
	{
		Criteria crit = getSession().createCriteria(BagAdres.class);
		crit.add(Restrictions.isNull("postcodeCoordinaten"));

		return crit;
	}
}
