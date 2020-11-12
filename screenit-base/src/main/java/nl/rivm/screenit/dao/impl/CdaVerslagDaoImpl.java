
package nl.rivm.screenit.dao.impl;

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

import nl.rivm.screenit.dao.CdaVerslagDao;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.SUPPORTS)
public class CdaVerslagDaoImpl extends AbstractAutowiredDao implements CdaVerslagDao
{

	@Override
	public boolean isBerichtReedsVerwerkt(String berichtId)
	{
		Criteria criteria = getSession().createCriteria(OntvangenCdaBericht.class);
		criteria.add(Restrictions.eq("berichtId", berichtId));
		criteria.add(Restrictions.or(Restrictions.eq("status", BerichtStatus.VERWERKING), Restrictions.eq("status", BerichtStatus.VERWERKT)));

		List<?> list = criteria.list();
		if (list == null || list.isEmpty())
		{
			return false;
		}
		return true;
	}

	@Override
	public boolean isBerichtReedsOntvangen(String setId, Long versie)
	{
		Criteria criteria = getSession().createCriteria(OntvangenCdaBericht.class);
		criteria.add(Restrictions.eq("setId", setId));
		criteria.add(Restrictions.ge("versie", versie));
		criteria.add(Restrictions.ne("status", BerichtStatus.FOUT));
		criteria.add(Restrictions.ne("status", BerichtStatus.WAARSCHUWING));

		List<?> list = criteria.list();
		if (list == null || list.isEmpty())
		{
			return false;
		}
		return true;
	}

	@Override
	public Verslag getVerslag(String setId, Class<? extends Verslag<?, ?>> clazz)
	{
		Criteria criteria = getSession().createCriteria(clazz);
		criteria.createAlias("ontvangenCdaBericht", "ontvangenCdaBericht");
		criteria.add(Restrictions.eq("ontvangenCdaBericht.setId", setId));
		criteria.add(Restrictions.eq("ontvangenCdaBericht.status", BerichtStatus.VERWERKT));

		List<?> list = criteria.list();
		if (list == null || list.isEmpty())
		{
			return null;
		}
		return (Verslag) list.get(0);
	}
}
