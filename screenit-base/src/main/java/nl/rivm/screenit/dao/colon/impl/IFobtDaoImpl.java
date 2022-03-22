package nl.rivm.screenit.dao.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;

import nl.rivm.screenit.dao.colon.IFobtDao;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.hibernate.envers.AuditReader;
import org.hibernate.envers.AuditReaderFactory;
import org.hibernate.envers.query.AuditEntity;
import org.hibernate.envers.query.AuditQuery;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class IFobtDaoImpl extends AbstractAutowiredDao implements IFobtDao
{

	@Override
	public IFOBTTest getIfobtTest(String barcode)
	{
		var crit = getSession().createCriteria(IFOBTTest.class);
		crit.add(Restrictions.eq("barcode", barcode));
		return (IFOBTTest) crit.uniqueResult();
	}

	@Override
	public IFOBTBestand getIfobtBestand(String bestandsNaam)
	{
		var crit = getSession().createCriteria(IFOBTBestand.class);
		crit.add(Restrictions.eq("naamBestand", bestandsNaam));
		crit.add(Restrictions.or(Restrictions.eq("status", IFOBTBestandStatus.NIEUW), Restrictions.eq("status", IFOBTBestandStatus.KAN_ORIG_BESTAND_VERWIJDEREN)));
		return (IFOBTBestand) crit.uniqueResult();
	}

	@Override
	public IFOBTTest getLaatsteIfobtTestMetMissendeUitslagVanDossier(ColonDossier dossier, LocalDate signalerenVanaf, LocalDate minimaleSignaleringsDatum)
	{
		var crit = getSession().createCriteria(IFOBTTest.class, "ifobt");
		ColonRestrictions.addIfobtMissendeUitslagRestrictions(crit, signalerenVanaf, minimaleSignaleringsDatum);
		crit.add(Restrictions.eq("ronde.dossier", dossier));
		crit.addOrder(Order.desc("analyseDatum"));
		crit.setMaxResults(1);
		return (IFOBTTest) crit.uniqueResult();
	}

	@Override
	public boolean isVerwijderdeBarcode(String barcode)
	{
		AuditReader reader = AuditReaderFactory.get(getSession());
		AuditQuery auditQuery = reader.createQuery().forRevisionsOfEntity(IFOBTTest.class, false, true)
			.add(AuditEntity.property("barcode").eq(barcode))
			.addProjection(AuditEntity.id().count());

		return ((Long) auditQuery.getSingleResult()) > 0;
	}
}
