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

import java.util.Collection;
import java.util.List;

import nl.rivm.screenit.mamma.planning.dao.PlanningStandplaatsRondeDao;
import nl.rivm.screenit.mamma.planning.dao.dto.CapaciteitBeschikbaarVoorDaoDto;
import nl.rivm.screenit.mamma.planning.dao.dto.StandplaatsRondeDaoDto;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.hibernate.Criteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.transform.Transformers;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Repository;

@Repository
public class PlanningStandplaatsRondeDaoImpl extends AbstractAutowiredDao implements PlanningStandplaatsRondeDao
{
	@Override
	public List<StandplaatsRondeDaoDto> standplaatsRondenVoorConceptmodel(Collection<Long> standplaatsRondeIds)
	{
		var criteria = standplaatsRondenVoorIdsCriteria(standplaatsRondeIds);

		criteria.setProjection(Projections.projectionList()
			.add(Projections.property("standplaatsRonde.id"), "id")
			.add(Projections.property("standplaatsRonde.interval"), "interval")
			.add(Projections.property("standplaatsRonde.standplaats.id"), "standplaatsId")
			.add(Projections.property("standplaatsRonde.afspraakDrempel"), "afspraakDrempel")
			.add(Projections.property("standplaatsRonde.achtervangStandplaats.id"), "achtervangStandplaatsId")
			.add(Projections.property("standplaatsRonde.minderValideUitwijkStandplaats.id"), "minderValideUitwijkStandplaatsId")
			.add(Projections.property("standplaatsRonde.achtervangToegepast"), "achtervangToegepast")
			.add(Projections.property("standplaatsRonde.minderValideUitnodigenVanaf"), "minderValideUitnodigenVanaf")
			.add(Projections.property("standplaatsRonde.extraMinderValideCapaciteitUitgenodigd"), "extraMinderValideCapaciteitUitgenodigd")
		);

		criteria.setResultTransformer(Transformers.aliasToBean(StandplaatsRondeDaoDto.class));

		return criteria.list();
	}

	@NotNull
	private Criteria standplaatsRondenVoorIdsCriteria(Collection<Long> standplaatsRondeIds)
	{
		var criteria = getSession().createCriteria(MammaStandplaatsRonde.class, "standplaatsRonde");
		criteria.add(Restrictions.in("standplaatsRonde.id", standplaatsRondeIds));
		return criteria;
	}

	@Override
	public List<CapaciteitBeschikbaarVoorDaoDto> capaciteitBeschikbaarVoorScreeningOrganisaties(Collection<Long> standplaatsRondeIds)
	{
		var criteria = standplaatsRondenVoorIdsCriteria(standplaatsRondeIds);

		criteria.createAlias("afspraakcapaciteitBeschikbaarVoor", "afspraakcapaciteitBeschikbaarVoor");
		criteria.add(Restrictions.isNotEmpty("standplaatsRonde.afspraakcapaciteitBeschikbaarVoor"));

		criteria.setProjection(Projections.projectionList()
			.add(Projections.property("standplaatsRonde.id"), "standplaatsRondeId")
			.add(Projections.property("afspraakcapaciteitBeschikbaarVoor.id"), "screeningOrganisatieId")
		);

		criteria.setResultTransformer(Transformers.aliasToBean(CapaciteitBeschikbaarVoorDaoDto.class));

		return criteria.list();
	}

}
