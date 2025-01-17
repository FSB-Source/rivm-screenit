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

import java.util.List;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.Root;

import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaMammograaf;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import static javax.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.BeoordelingsEenheidSpecification.heeftScreeningOrganisatieId;
import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.isActief;

public interface PlanningScreeningsEenheidRepository extends BaseJpaRepository<MammaScreeningsEenheid>
{
	default List<PlanningScreeningsEenheid> findActieveScreeningsEenhedenVanScreeningsOrganisatie(long screeningsOrganisatieId)
	{
		var specs = isActief().and(heeftScreeningOrganisatieId(screeningsOrganisatieId).with(MammaScreeningsEenheid_.beoordelingsEenheid));

		return findWith(specs, PlanningScreeningsEenheid.class, q -> q.projections((cb, r) ->
				List.of(r.get(AbstractHibernateObject_.id),
					r.get(MammaScreeningsEenheid_.uitgenodigdTotEnMet),
					r.get(MammaScreeningsEenheid_.uitnodigenTotEnMet),
					r.get(MammaScreeningsEenheid_.interval),
					r.get(MammaScreeningsEenheid_.herhalingsWeek),
					cb.countDistinct(getMammografenJoin(r).get(AbstractHibernateObject_.id)),
					cb.max(getStandplaatsPeriodenJoin(r).get(MammaStandplaatsPeriode_.screeningsEenheidVolgNr))))
			.groupBy((cb, r) -> List.of(r.get(AbstractHibernateObject_.id)))
			.all());
	}

	private Join<?, MammaStandplaatsPeriode> getStandplaatsPeriodenJoin(Root<MammaScreeningsEenheid> r)
	{
		return join(r, MammaScreeningsEenheid_.standplaatsPerioden, LEFT);
	}

	private Join<?, MammaMammograaf> getMammografenJoin(Root<MammaScreeningsEenheid> r)
	{
		return join(r, MammaScreeningsEenheid_.mammografen, LEFT);
	}
}
