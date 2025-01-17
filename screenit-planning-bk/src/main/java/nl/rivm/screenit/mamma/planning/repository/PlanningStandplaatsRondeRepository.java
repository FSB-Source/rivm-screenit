package nl.rivm.screenit.mamma.planning.repository;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.mamma.planning.repository.projectie.CapaciteitBeschikbaarVoorProjectie;
import nl.rivm.screenit.mamma.planning.repository.projectie.StandplaatsRondeProjectie;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;
import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftIdIn;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaStandplaatsRondeSpecification.heeftAfspraakcapaciteitBeschikbaarVoor;

public interface PlanningStandplaatsRondeRepository extends BaseJpaRepository<MammaStandplaatsRonde>
{
	default List<StandplaatsRondeProjectie> findStandplaatsRondenVoorConceptmodel(Collection<Long> standplaatsRondeIds)
	{
		return findWith(heeftIdIn(standplaatsRondeIds), StandplaatsRondeProjectie.class, q -> q.projections((cb, r) ->
			List.of(
				r.get(AbstractHibernateObject_.id),
				r.get(MammaStandplaatsRonde_.interval),
				r.get(MammaStandplaatsRonde_.standplaats).get(AbstractHibernateObject_.id),
				r.get(MammaStandplaatsRonde_.afspraakDrempel),
				r.get(MammaStandplaatsRonde_.achtervangStandplaats).get(AbstractHibernateObject_.id),
				r.get(MammaStandplaatsRonde_.minderValideUitwijkStandplaats).get(AbstractHibernateObject_.id),
				r.get(MammaStandplaatsRonde_.achtervangToegepast),
				r.get(MammaStandplaatsRonde_.minderValideUitnodigenVanaf),
				r.get(MammaStandplaatsRonde_.extraMinderValideCapaciteitUitgenodigd)
			)
		).all());
	}

	default List<CapaciteitBeschikbaarVoorProjectie> findCapaciteitBeschikbaarVoorScreeningOrganisaties(Collection<Long> standplaatsRondeIds)
	{
		return findWith(heeftAfspraakcapaciteitBeschikbaarVoor().and(heeftIdIn(standplaatsRondeIds)), CapaciteitBeschikbaarVoorProjectie.class, q -> q.projections((cb, r) ->
			List.of(
				r.get(AbstractHibernateObject_.id),
				join(r, MammaStandplaatsRonde_.afspraakcapaciteitBeschikbaarVoor).get(SingleTableHibernateObject_.id)
			)
		).all());
	}
}
