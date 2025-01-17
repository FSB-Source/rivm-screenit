package nl.rivm.screenit.batch.repository;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import nl.rivm.screenit.model.logging.MammaIlmLogEvent;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIlmBeeldenStatusRapportage;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;

public interface MammaIlmLogEventRepository extends BaseJpaRepository<MammaIlmLogEvent>
{
	@Query("SELECT ilmLogEvent.rapportage FROM MammaIlmLogEvent ilmLogEvent " +
		"JOIN ilmLogEvent.logRegel logRegel " +
		"ORDER BY logRegel.gebeurtenisDatum DESC")
	List<MammaIlmBeeldenStatusRapportage> haalRapportagesOp(Pageable pageable);
}