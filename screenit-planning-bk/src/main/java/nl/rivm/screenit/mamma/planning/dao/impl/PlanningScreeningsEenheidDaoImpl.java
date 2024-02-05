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

import java.math.BigDecimal;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.mamma.planning.dao.PlanningScreeningsEenheidDao;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.sql.JoinType;
import org.hibernate.transform.ResultTransformer;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class PlanningScreeningsEenheidDaoImpl extends AbstractAutowiredDao implements PlanningScreeningsEenheidDao
{
	private final ICurrentDateSupplier dateSupplier;

	@Override
	public Map<Long, Integer> volgnummersEersteActieveStandplaatsPeriodePerSe()
	{
		Criteria criteria = getSession().createCriteria(MammaScreeningsEenheid.class, "screeningsEenheid");
		criteria.add(Restrictions.eq("screeningsEenheid.actief", true));

		criteria.createAlias("screeningsEenheid.standplaatsPerioden", "standplaatsPeriode");
		criteria.add(Restrictions.ge("standplaatsPeriode.totEnMet", dateSupplier.getDateMidnight()));

		criteria.createAlias("standplaatsPeriode.standplaatsRonde", "standplaatsRonde");
		criteria.createAlias("standplaatsRonde.standplaats", "standplaats");
		criteria.add(Restrictions.eq("standplaats.actief", true));

		criteria.createAlias("standplaatsRonde.standplaatsPerioden", "standplaatsPeriode2");

		criteria.setProjection(Projections.projectionList()
			.add(Projections.groupProperty("standplaatsPeriode2.screeningsEenheid.id")) 
			.add(Projections.min("standplaatsPeriode2.screeningsEenheidVolgNr"))); 
		List<Object[]> queryResult = criteria.list();

		var volgnummerPerSe = new HashMap<Long, Integer>();
		queryResult.forEach(se -> volgnummerPerSe.put((Long) se[0], (Integer) se[1]));

		return volgnummerPerSe;
	}

	@Override
	public List<PlanningScreeningsEenheid> actieveScreeningsEenhedenVanScreeningsOrganisatie(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		var criteria = getSession().createCriteria(MammaScreeningsEenheid.class, "screeningsEenheid");
		criteria.createAlias("screeningsEenheid.beoordelingsEenheid", "beoordelingsEenheid");
		criteria.createAlias("beoordelingsEenheid.parent", "centraleEenheid");
		criteria.createAlias("screeningsEenheid.standplaatsPerioden", "standplaatsPerioden", JoinType.LEFT_OUTER_JOIN);
		criteria.createAlias("screeningsEenheid.mammografen", "mammografen", JoinType.LEFT_OUTER_JOIN);

		criteria.add(Restrictions.eq("screeningsEenheid.actief", true));
		criteria.add(Restrictions.eq("centraleEenheid.regio.id", screeningsOrganisatie.getId()));

		criteria.setProjection(Projections.projectionList()
			.add(Projections.groupProperty("screeningsEenheid.id")) 
			.add(Projections.property("screeningsEenheid.uitgenodigdTotEnMet")) 
			.add(Projections.property("screeningsEenheid.uitnodigenTotEnMet")) 
			.add(Projections.countDistinct("mammografen.id")) 
			.add(Projections.property("screeningsEenheid.interval")) 
			.add(Projections.property("screeningsEenheid.herhalingsWeek")) 
			.add(Projections.max("standplaatsPerioden.screeningsEenheidVolgNr"))); 

		criteria.setResultTransformer(new PlanningScreeningsEendheidResultTransformer());
		return criteria.list();
	}

	private static class PlanningScreeningsEendheidResultTransformer implements ResultTransformer
	{
		@Override
		public PlanningScreeningsEenheid transformTuple(Object[] tuple, String[] aliases)
		{
			var screeningsEenheidId = (Long) tuple[0];
			var uitgenodigdTotEnMet = DateUtil.toLocalDate((Date) tuple[1]);
			var uitnodigenTotEnMet = DateUtil.toLocalDate((Date) tuple[2]);
			var interval = (BigDecimal) tuple[4];
			var herhalingsWeekMaandag = DateUtil.toLocalDate((Date) tuple[5]);

			var screeningsEenheid = new PlanningScreeningsEenheid(screeningsEenheidId, uitgenodigdTotEnMet, uitnodigenTotEnMet, interval, herhalingsWeekMaandag);

			var aantalMammografen = (Long) tuple[3];
			screeningsEenheid.setAantalMammografen((int) Math.max(aantalMammografen, 1)); 

			Integer maxStandplaatsPeriodeVolgnummer = (Integer) tuple[6];
			if (maxStandplaatsPeriodeVolgnummer != null)
			{
				screeningsEenheid.setVolgNrOffset(maxStandplaatsPeriodeVolgnummer + 1);
			}

			return screeningsEenheid;
		}

		@Override
		public List<PlanningScreeningsEenheid> transformList(List list)
		{
			return list;
		}
	}
}
