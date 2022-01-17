
package nl.rivm.screenit.dao.impl;

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

import java.util.Date;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.KwaliteitsovereenkomstDao;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;
import nl.topicuszorg.hibernate.restrictions.NvlRestrictions;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class KwaliteitsovereenkomstDaoImpl extends AbstractAutowiredDao implements KwaliteitsovereenkomstDao
{

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public boolean hasActiveKwaliteitsovereenkomst(Gebruiker gebruiker, Date overeenkomstPeildatum)
	{
		if (overeenkomstPeildatum == null)
		{
			overeenkomstPeildatum = currentDateSupplier.getDate();
		}
		Criteria crit = getSession().createCriteria(AfgeslotenMedewerkerOvereenkomst.class);
		crit.add(Restrictions.eq("gebruiker", gebruiker));
		crit.add(Restrictions.le("startDatum", overeenkomstPeildatum));
		crit.add(NvlRestrictions.ge("eindDatum", overeenkomstPeildatum, Constants.END_OF_TIME));
		crit.setProjection(Projections.rowCount());
		return ((Long) crit.uniqueResult()).longValue() > 0;
	}

}
