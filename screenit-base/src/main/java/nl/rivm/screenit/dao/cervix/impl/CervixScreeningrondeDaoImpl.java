package nl.rivm.screenit.dao.cervix.impl;

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

import nl.rivm.screenit.dao.cervix.CervixScreeningrondeDao;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixScreeningrondeDaoImpl extends AbstractAutowiredDao implements CervixScreeningrondeDao
{
	@Override
	public CervixScreeningRonde getLaatsteScreeningRonde(String bsn)
	{
		Criteria criteria = getSession().createCriteria(CervixScreeningRonde.class, "ronde");
		criteria.createAlias("ronde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");

		criteria.add(Restrictions.eq("persoon.bsn", bsn));

		criteria.addOrder(Order.desc("ronde.creatieDatum"));
		criteria.setMaxResults(1);

		return (CervixScreeningRonde) criteria.uniqueResult();
	}

	@Override
	public List<CervixUitnodiging> getTeHerinnerenUitnodigingen(CervixScreeningRonde ronde)
	{
		Criteria criteria = getSession().createCriteria(CervixUitnodiging.class, "uitnodiging");
		criteria.createAlias("uitnodiging.screeningRonde", "ronde");

		criteria.add(Restrictions.eq("ronde.id", ronde.getId()));
		criteria.add(Restrictions.eq("uitnodiging.herinneren", true));
		criteria.add(Restrictions.isNull("uitnodiging.herinnerenGeannuleerdDatum"));

		return criteria.list();
	}
}
