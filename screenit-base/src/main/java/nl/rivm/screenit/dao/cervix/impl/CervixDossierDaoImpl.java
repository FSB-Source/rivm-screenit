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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.cervix.CervixDossierDao;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixDossierDaoImpl extends AbstractAutowiredDao implements CervixDossierDao
{

	@Override
	public CervixScreeningRonde getOntvangstRonde(CervixMonster monster)
	{
		Date peildatum = monster.getOntvangstdatum();
		monster = (CervixMonster) HibernateHelper.deproxy(monster);
		if (monster instanceof CervixUitstrijkje)
		{
			CervixLabformulier labformulier = ((CervixUitstrijkje) monster).getLabformulier();
			if (labformulier != null
				&& (peildatum == null || DateUtil.compareBefore(labformulier.getScanDatum(), peildatum))
				&& (labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD
					|| labformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE
					|| labformulier.getStatus() == CervixLabformulierStatus.HUISARTS_ONBEKEND))
			{
				peildatum = labformulier.getScanDatum();
			}
		}

		if (peildatum != null)
		{
			Criteria criteria = getSession().createCriteria(CervixScreeningRonde.class, "ontvangstRonde");
			criteria.createAlias("ontvangstRonde.dossier", "dossier");
			criteria.createAlias("dossier.screeningRondes", "ronde");
			criteria.createAlias("ronde.uitnodigingen", "uitnodiging");
			criteria.createAlias("uitnodiging.monster", "monster");
			criteria.add(Restrictions.eq("monster.id", monster.getId()));

			criteria.add(Restrictions.lt("ontvangstRonde.creatieDatum", peildatum));

			criteria.addOrder(Order.desc("ontvangstRonde.id")).setFirstResult(0).setMaxResults(1);
			List<CervixScreeningRonde> ronde = criteria.list();
			if (ronde.isEmpty())
			{
				throw new IllegalStateException("nog.geen.ronde.op.peildatum");
			}
			return ronde.get(0);
		}
		return null;
	}
}
