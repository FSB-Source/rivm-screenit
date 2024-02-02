
package nl.rivm.screenit.dao.impl;

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

import nl.rivm.screenit.dao.BaseUitnodigingDao;
import nl.rivm.screenit.model.Uitnodiging;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.springframework.stereotype.Repository;

@Repository
public class BaseUitnodigingDaoImpl extends AbstractAutowiredDao implements BaseUitnodigingDao
{

	@Override
	public <U extends Uitnodiging<?>> boolean uitnodigingExists(Class<U> clazz, String trackId)
	{
		if (StringUtils.isBlank(trackId))
		{
			return false;
		}
		Criteria criteria = getSession().createCriteria(clazz);
		criteria.add(Restrictions.eq("trackTraceId", trackId));
		return CollectionUtils.isNotEmpty(criteria.list());
	}

	@Override
	public <U extends Uitnodiging<?>> U getUitnodiging(Class<U> clazz, String trackId, String postcode, Integer huisnummer)
	{
		Criteria criteria = getSession().createCriteria(clazz);
		criteria.createAlias("screeningRonde", "screeningRonde");
		criteria.createAlias("screeningRonde.dossier", "dossier");
		criteria.createAlias("dossier.client", "client");
		criteria.createAlias("client.persoon", "persoon");
		criteria.createAlias("persoon.gbaAdres", "gbaAdres");
		criteria.createAlias("persoon.tijdelijkAdres", "tijdelijkAdres", JoinType.LEFT_OUTER_JOIN);

		criteria.add(Restrictions.eq("trackTraceId", trackId));
		Disjunction disjunction = Restrictions.disjunction();
		criteria.add(disjunction);
		disjunction.add(Restrictions.and(Restrictions.eq("gbaAdres.postcode", postcode), Restrictions.eq("gbaAdres.huisnummer", huisnummer)));
		disjunction.add(Restrictions.and(Restrictions.eq("tijdelijkAdres.postcode", postcode), Restrictions.eq("tijdelijkAdres.huisnummer", huisnummer)));

		U uitnodiging = null;
		List list = criteria.list();
		if (CollectionUtils.isNotEmpty(list) && list.size() == 1)
		{
			uitnodiging = (U) list.get(0);
		}
		return uitnodiging;
	}

}
