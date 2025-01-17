package nl.rivm.screenit.repository.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;

import nl.rivm.screenit.model.cervix.facturatie.CervixBoekRegel;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface CervixBoekRegelRepository extends BaseJpaRepository<CervixBoekRegel>
{

	@Query("SELECT COALESCE(SUM(bor.bedrag), 0.0) "
		+ "FROM CervixBetaalopdracht bo "
		+ "JOIN CervixBetaalopdrachtRegel bor ON bor.betaalopdracht = bo "
		+ "WHERE bo.id = :boId and bor.laboratorium IS NOT NULL")
	BigDecimal totaalBedragLaboratoriumBoekRegels(@Param("boId") Long id);
}
