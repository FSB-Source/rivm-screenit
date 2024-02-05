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
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.planning.dao.PlanningCapaciteitBlokDao;
import nl.rivm.screenit.mamma.planning.model.PlanningBlok;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;

@Repository
public class PlanningCapaciteitBlokDaoImpl extends AbstractAutowiredDao implements PlanningCapaciteitBlokDao
{

	@Override
	public Set<PlanningBlok> leesCapaciteitBlokken(PlanningScreeningsEenheid screeningsEenheid, LocalDate vanaf, LocalDate totEnMet)
	{
		var crit = getSession().createCriteria(MammaCapaciteitBlok.class, "capaciteitBlok");
		crit.createAlias("capaciteitBlok.screeningsEenheid", "screeningsEenheid");

		crit.add(Restrictions.eq("screeningsEenheid.id", screeningsEenheid.getId()));

		if (vanaf != null)
		{
			crit.add(Restrictions.ge("capaciteitBlok.vanaf", DateUtil.toUtilDate(vanaf)));
		}
		if (totEnMet != null)
		{
			crit.add(Restrictions.le("capaciteitBlok.tot", DateUtil.toUtilDate(totEnMet.plusDays(1))));
		}

		crit.setProjection(Projections.projectionList()
			.add(Projections.property("capaciteitBlok.id")) 
			.add(Projections.property("capaciteitBlok.vanaf")) 
			.add(Projections.property("capaciteitBlok.tot")) 
			.add(Projections.property("capaciteitBlok.aantalOnderzoeken")) 
			.add(Projections.property("capaciteitBlok.blokType")) 
			.add(Projections.property("capaciteitBlok.opmerkingen")) 
			.add(Projections.property("capaciteitBlok.minderValideAfspraakMogelijk")) 
		);

		return ((List<Object[]>) crit.list()).stream().map((dbBlock -> mapNaarPlanningBlok(screeningsEenheid, dbBlock))).collect(Collectors.toSet());
	}

	private PlanningBlok mapNaarPlanningBlok(PlanningScreeningsEenheid screeningsEenheid, Object[] dbBlok)
	{
		var vanaf = DateUtil.toLocalDateTime((Date) dbBlok[1]);

		var blok = new PlanningBlok((Long) dbBlok[0], vanaf.toLocalTime(), DateUtil.toLocalTime((Date) dbBlok[2]),
			(Integer) dbBlok[3], (MammaCapaciteitBlokType) dbBlok[4], (String) dbBlok[5], (Boolean) dbBlok[6]);

		var dag = screeningsEenheid.getDagNavigableMap().get(vanaf.toLocalDate());
		blok.setDag(dag);
		blok.setScreeningsEenheid(screeningsEenheid);
		return blok;
	}

	@Override
	public Optional<LocalDate> findMaxDatumVanAlleCapaciteitsBlokken()
	{
		var criteria = getSession().createCriteria(MammaCapaciteitBlok.class, "capaciteitBlok");
		criteria.setProjection(Projections.max("capaciteitBlok.vanaf"));
		var max = (Date) criteria.uniqueResult();
		return Optional.ofNullable(DateUtil.toLocalDate(max));
	}
}
