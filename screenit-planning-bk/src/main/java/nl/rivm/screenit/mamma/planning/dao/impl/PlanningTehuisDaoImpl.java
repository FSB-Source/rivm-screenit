package nl.rivm.screenit.mamma.planning.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import nl.rivm.screenit.mamma.planning.dao.PlanningTehuisDao;
import nl.rivm.screenit.mamma.planning.dao.dto.TehuisDaoDto;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.transform.Transformers;
import org.springframework.stereotype.Repository;

@Repository
public class PlanningTehuisDaoImpl extends AbstractAutowiredDao implements PlanningTehuisDao
{
	@Override
	public List<TehuisDaoDto> actieveTehuizen()
	{
		var criteria = getSession().createCriteria(MammaTehuis.class, "tehuis");
		criteria.createAlias("tehuis.standplaats", "standplaats");

		criteria.add(Restrictions.eq("tehuis.actief", true));

		criteria.setProjection(Projections.projectionList()
			.add(Projections.property("tehuis.id"), "id")
			.add(Projections.property("tehuis.standplaats.id"), "standplaatsId")
			.add(Projections.property("standplaats.regio.id"), "screeningOrganisatieId")
		);

		criteria.setResultTransformer(Transformers.aliasToBean(TehuisDaoDto.class));
		return criteria.list();
	}
}
