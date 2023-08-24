package nl.rivm.screenit.dao.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.dao.cervix.CervixBMHKLaboratoriumDao;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.cervix.CervixCytologieOrder;
import nl.rivm.screenit.model.cervix.CervixHpvBericht;
import nl.topicuszorg.hibernate.criteria.BaseCriteria;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
public class CervixBMHKLaboratoriumDaoImpl extends AbstractAutowiredDao implements CervixBMHKLaboratoriumDao
{

	@Autowired
	private HibernateService hibernateService;

	@Override
	public BMHKLaboratorium getBmhkLaboratoriumfromUserIdScanner(String userIdScanner)
	{
		BaseCriteria<BMHKLaboratorium> baseCriteria = new BaseCriteria<>(BMHKLaboratorium.class, "bmhkLaboratorium");
		baseCriteria.createAlias("bmhkLaboratorium.userIdScanners", "userIdScanners");
		baseCriteria.add(Restrictions.eq("userIdScanners.elements", userIdScanner));
		return baseCriteria.uniqueResult(getSession());
	}

	@Override
	public BMHKLaboratorium getBmhkLaboratoriumfromZInstrumentNames(String zInstrumentName)
	{
		BaseCriteria<BMHKLaboratorium> baseCriteria = new BaseCriteria<>(BMHKLaboratorium.class, "bmhkLaboratorium");
		baseCriteria.createAlias("bmhkLaboratorium.instrumentNames", "instrumentNames");
		baseCriteria.add(Restrictions.eq("instrumentNames.elements", zInstrumentName));
		return baseCriteria.uniqueResult(getSession());
	}

	@Override
	public boolean isHpvBerichtAlOntvangen(String messageId)
	{
		Criteria crit = getSession().createCriteria(CervixHpvBericht.class);
		crit.add(Restrictions.eq("messageId", messageId));
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue() > 0;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateLaboratorium(BMHKLaboratorium laboratorium, List<Gemeente> gemeentes)
	{
		List<Gemeente> gekoppeldeGemeentes = laboratorium.getGemeentes();
		for (Gemeente gemeente : gekoppeldeGemeentes)
		{
			gemeente.setBmhkLaboratorium(laboratorium);
			hibernateService.saveOrUpdate(gemeente);
		}
		for (Gemeente gemeente : gemeentes)
		{
			if (!gekoppeldeGemeentes.contains(gemeente) && laboratorium.equals(gemeente.getBmhkLaboratorium()))
			{

				gemeente.setBmhkLaboratorium(null);
				hibernateService.saveOrUpdate(gemeente);
			}
		}

		hibernateService.saveOrUpdate(laboratorium);
	}

	@Override
	public CervixCytologieOrder getCytologieOrder(String monsterId)
	{
		BaseCriteria<CervixCytologieOrder> baseCriteria = new BaseCriteria<>(CervixCytologieOrder.class, "cytologieOrder");
		baseCriteria.createAlias("cytologieOrder.uitstrijkje", "uitstrijkje");
		baseCriteria.add(Restrictions.eq("uitstrijkje.monsterId", monsterId));
		return baseCriteria.uniqueResult(getSession());
	}
}
