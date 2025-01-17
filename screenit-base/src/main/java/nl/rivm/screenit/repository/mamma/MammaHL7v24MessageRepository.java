package nl.rivm.screenit.repository.mamma;

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

import nl.rivm.screenit.model.mamma.MammaHL7v24Message;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface MammaHL7v24MessageRepository extends BaseJpaRepository<MammaHL7v24Message>
{
	@Modifying
	@Query(nativeQuery = true, value = "delete from mamma.hl7v24_message h7v24m where h7v24m.dto_json like ('%\"clientId\":' || :clientId || '%')")
	void verwijderAlleBerichtenVanClient(long clientId);

	@Modifying
	@Query(nativeQuery = true, value = "delete from mamma.hl7v24_message h7v24m where h7v24m.dto_json like ('%\"clientId\":' || :clientId || '%')"
		+ "and h7v24m.dto_json not like ('%\"status\":\"' || :deleteOrmStatus || '\"%') and h7v24m.dto_json not like ('%\"status\":\"' || :goingToDeleteOrm || '\"%')")
	void verwijderAlleBerichtenExclusiefImsVoorClient(long clientId, String deleteOrmStatus, String goingToDeleteOrm);
}
