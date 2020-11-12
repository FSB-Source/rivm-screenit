package nl.rivm.screenit.dao.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;

import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.colon.IFOBTUitslag;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.berichten.ColonIFobtUitslagBericht;
import nl.rivm.screenit.dao.colon.IFOBTResultDao;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.REQUIRED)
public class IFOBTResultDaoImpl extends AbstractAutowiredDao implements IFOBTResultDao
{
	@Override
	public IFobtLaboratorium getIFobtLaboratorium(String labId)
	{
		Criteria crit = this.getSession().createCriteria(IFobtLaboratorium.class);
		crit.add(Restrictions.eq("labId", labId));
		return (IFobtLaboratorium) crit.uniqueResult();
	}

	@Override
	public List<ColonIFobtUitslagBericht> getAlleNietVerwerkteHpvBerichten()
	{
		Criteria crit = getSession().createCriteria(ColonIFobtUitslagBericht.class);
		crit.add(Restrictions.eq("status", BerichtStatus.NIEUW));
		return crit.list();
	}

	@Override
	public boolean isIFobtBerichtAlOntvangen(String messageId)
	{
		Criteria crit = getSession().createCriteria(ColonIFobtUitslagBericht.class);
		crit.add(Restrictions.eq("messageId", messageId));
		crit.setProjection(Projections.rowCount());
		return ((Number) crit.uniqueResult()).longValue() > 0;
	}

	@Override
	public Boolean fitUitslagGeanalyseerdMaarNietVerwerkt(String barcode)
	{
		Criteria crit = this.getSession().createCriteria(IFOBTUitslag.class);
		crit.add(Restrictions.eq("barcode", barcode));
		crit.createAlias("bestand", "bestand");
		crit.add(Restrictions.ne("bestand.status", IFOBTBestandStatus.VERWERKT));
		return crit.list().size() > 0;
	}
}
