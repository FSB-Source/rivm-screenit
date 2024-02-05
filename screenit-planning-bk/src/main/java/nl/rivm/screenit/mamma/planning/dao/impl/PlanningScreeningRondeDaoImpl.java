package nl.rivm.screenit.mamma.planning.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.time.LocalDate;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.mamma.planning.dao.PlanningScreeningRondeDao;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

@Repository
public class PlanningScreeningRondeDaoImpl extends AbstractAutowiredDao implements PlanningScreeningRondeDao
{
	@Override
	public Map<Long, LocalDate> vorigeScreeningRondeDatumPerDossier()
	{
		var criteria = getSession().createCriteria(MammaScreeningRonde.class, "screeningRonde");
		criteria.createAlias("screeningRonde.dossier", "dossier");
		criteria.add(Restrictions.neProperty("screeningRonde.id", "dossier.laatsteScreeningRonde"));

		criteria.setProjection(Projections.projectionList()
			.add(Projections.groupProperty("dossier.id")) 
			.add(Projections.max("screeningRonde.creatieDatum"))); 

		var vorigeScreeningRondeCreatieDatumMap = new HashMap<Long, LocalDate>();
		List<Object[]> queryResult = criteria.list();
		queryResult.forEach(result -> vorigeScreeningRondeCreatieDatumMap.put((Long) result[0], DateUtil.toLocalDate((Date) result[1])));

		return vorigeScreeningRondeCreatieDatumMap;
	}
}
