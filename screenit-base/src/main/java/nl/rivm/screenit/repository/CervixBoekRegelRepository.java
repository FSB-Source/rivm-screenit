package nl.rivm.screenit.repository;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.criteria.Join;

import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegelSpecificatie;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegelSpecificatie_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdrachtRegel_;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel_;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

@Repository
public interface CervixBoekRegelRepository extends JpaRepository<CervixBoekRegel, Long>, JpaSpecificationExecutor<CervixBoekRegel>
{
	default Specification<CervixBoekRegel> baseSpecification()
	{
		return ((r, q, cb) -> cb.isNotNull(r));
	}

	default Specification<CervixBoekRegel> metSpecificatie()
	{
		return (root, query, criteriaBuilder) -> criteriaBuilder.isNotNull(root.get(CervixBoekRegel_.specificatie));
	}

	default Specification<CervixBoekRegel> metOpdrachtID(Long id)
	{
		return (root, query, criteriaBuilder) -> {
			Join<CervixBoekRegel, CervixBetaalopdrachtRegelSpecificatie> specificatieJoin = root.join(CervixBoekRegel_.specificatie);
			Join<CervixBetaalopdrachtRegelSpecificatie, CervixBetaalopdrachtRegel> regelJoin = specificatieJoin.join(CervixBetaalopdrachtRegelSpecificatie_.betaalopdrachtRegel);
			Join<CervixBetaalopdrachtRegel, CervixBetaalopdracht> betaalopdrachtJoin = regelJoin.join(CervixBetaalopdrachtRegel_.betaalopdracht);

			return criteriaBuilder.equal(betaalopdrachtJoin.get(AbstractHibernateObject_.id), id);
		};
	}
}
