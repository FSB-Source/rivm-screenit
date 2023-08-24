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
import java.util.Map;

import nl.rivm.screenit.mamma.planning.dao.PlanningStandplaatsPeriodeDao;
import nl.rivm.screenit.mamma.planning.dao.dto.StandplaatsPeriodeDaoDto;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.transform.Transformers;
import org.springframework.stereotype.Repository;

@Repository
public class PlanningStandplaatsPeriodeDaoImpl extends AbstractAutowiredDao implements PlanningStandplaatsPeriodeDao
{
	@Override
	public List<StandplaatsPeriodeDaoDto> standplaatsPeriodesVanafVolgnummer(Map<Long, Integer> eersteVolgnummerPerSe)
	{
		var criteria = getSession().createCriteria(MammaStandplaatsPeriode.class, "standplaatsPeriode");
		criteria.createAlias("standplaatsPeriode.standplaatsRonde", "standplaatsRonde");
		criteria.createAlias("standplaatsRonde.standplaats", "standplaats");
		criteria.createAlias("standplaats.regio", "regio");

		var or = Restrictions.disjunction();
		eersteVolgnummerPerSe.forEach((se, volnummer) ->
			or.add(Restrictions.and(
				Restrictions.eq("standplaatsPeriode.screeningsEenheid.id", se),
				Restrictions.ge("standplaatsPeriode.screeningsEenheidVolgNr", volnummer)))
		);
		criteria.add(or);

		criteria.setProjection(Projections.projectionList()
			.add(Projections.property("standplaatsPeriode.vanaf"), "vanafUtilDate")
			.add(Projections.property("standplaatsPeriode.totEnMet"), "totEnMetUtilDate")
			.add(Projections.property("standplaatsPeriode.id"), "standplaatsPeriodeId")
			.add(Projections.property("standplaatsRonde.id"), "standplaatsRondeId")
			.add(Projections.property("regio.wekenVanTevorenUitnodigen"), "wekenVanTevorenUitnodigen")
			.add(Projections.property("regio.id"), "regioId"));

		criteria.setResultTransformer(Transformers.aliasToBean((StandplaatsPeriodeDaoDto.class)));

		return criteria.list();
	}
}
