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

import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.rivm.screenit.specification.mamma.MammaStandplaatsSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import static nl.rivm.screenit.specification.mamma.MammaStandplaatsSpecification.isActief;

public interface PlanningStandplaatsRepository extends BaseJpaRepository<MammaStandplaats>
{
	default List<Long> findActieveStandplaatsIds(long screeningsOrganisatieId)
	{
		return findWith(isActief().and(MammaStandplaatsSpecification.heeftScreeningOrganisatieId(screeningsOrganisatieId)), Long.class,
			q -> q.projection((cb, r) -> r.get(AbstractHibernateObject_.id)).all());
	}
}
