package nl.rivm.screenit.dao.mamma.impl;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.dao.mamma.MammaBaseOnderzoekDao;
import nl.rivm.screenit.dao.mamma.MammaMissendeUitslagenRestrictions;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@AllArgsConstructor
public class MammaBaseOnderzoekDaoImpl extends AbstractAutowiredDao implements MammaBaseOnderzoekDao
{

	private final MammaMissendeUitslagenRestrictions missendeUitslagenRestrictions;

	@Override
	public MammaOnderzoek getLaatsteOnderzoekMetMissendeUitslagVanDossier(MammaDossier dossier)
	{
		var criteria = getSession().createCriteria(MammaOnderzoek.class, "onderzoek");
		missendeUitslagenRestrictions.addBeeldenZonderUitslagRestrictions(criteria);
		criteria.add(Restrictions.eq("ronde.dossier", dossier));
		criteria.addOrder(Order.desc("onderzoek.afgerondOp"));
		criteria.setMaxResults(1);
		return (MammaOnderzoek) criteria.uniqueResult();
	}

}
