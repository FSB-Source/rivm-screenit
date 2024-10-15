package nl.rivm.screenit.repository.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;

import javax.persistence.Tuple;

import nl.rivm.screenit.model.colon.planning.ColonTijdslot;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface ColonTijdslotRepository extends BaseJpaRepository<ColonTijdslot>
{
	@Query(nativeQuery = true, value = "with afspraakslots as (select t.id, t.vanaf as startDatum, t.tot as eindDatum, "
		+ "k.naam as kamer, k.id as kamerId, "
		+ "cast (t.vanaf as time) as startTijd, "
		+ "cast (t.tot as time) as eindTijd, "
		+ "extract(DOW from t.vanaf) as dag "
		+ "from colon.tijdslot t "
		+ "inner join colon.intakekamer k on t.kamer = k.id "
		+ "inner join algemeen.org_organisatie il on il.id = k.intakelocatie "
		+ "where il.id = :intakelocatieId "
		+ "and il.actief = true "
		+ "and k.actief = true "
		+ "and (k.id = :kamerId or :kamerId is null)"
		+ "and vanaf >= :startDatum "
		+ "and vanaf < :eindDatum "
		+ "and t.type = :typeTijdslot)"
		+ "select id as tijdslotId, startDatum, eindDatum, kamerId, kamer "
		+ "from afspraakslots "
		+ "where eindTijd>:startTijd "
		+ "and startTijd<:eindTijd "
		+ "and dag in (:dagen) "
		+ "order by startDatum, kamerId;")
	List<Tuple> searchTijdslots(@Param("intakelocatieId") Long intakelocatieId, @Param("startDatum") LocalDateTime startDatum,
		@Param("eindDatum") LocalDateTime eindDatum, @Param("startTijd") LocalTime startTijd, @Param("eindTijd") LocalTime eindTijd, @Param("kamerId") Long kamerId,
		@Param("dagen") List<Integer> dagen, @Param("typeTijdslot") String typeTijdslot);
}
