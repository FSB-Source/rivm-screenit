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

import java.util.List;

import nl.rivm.screenit.mamma.planning.dao.PlanningStandplaatsDao;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

@Repository
public class PlanningStandplaatsDaoImpl extends AbstractAutowiredDao implements PlanningStandplaatsDao
{
	@Override
	public List<Long> findActieveStandplaatsIds(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		var criteria = getSession().createCriteria(MammaStandplaats.class, "standplaats");
		criteria.createAlias("standplaats.regio", "screeningsOrganisatie");

		criteria.add(Restrictions.eq("standplaats.actief", true));
		criteria.add(Restrictions.eq("screeningsOrganisatie.id", screeningsOrganisatie.getId()));

		criteria.setProjection(Projections.id());

		return criteria.list();
	}
}
