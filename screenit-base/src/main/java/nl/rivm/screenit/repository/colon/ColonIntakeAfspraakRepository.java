package nl.rivm.screenit.repository.colon;

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

import java.time.LocalTime;
import java.util.List;

import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface ColonIntakeAfspraakRepository extends BaseJpaRepository<ColonIntakeAfspraak>
{
	@Query(nativeQuery = true, value = "with time_table as ("
		+ " select make_time(cast(extract(HOUR from papp.vanaf) as INTEGER), cast(extract(MINUTE from papp.vanaf) as INTEGER), cast (extract(SECOND from papp.vanaf) as DOUBLE PRECISION)) as start_time,"
		+ " make_time(cast(extract(HOUR from papp.tot) as INTEGER), cast(extract(MINUTE from papp.tot) as INTEGER), cast (extract(SECOND from papp.tot) as DOUBLE PRECISION)) as end_time"
		+ " from colon.afspraakslot  afs"
		+ " inner join colon.tijdslot papp on afs.id=papp.id"
		+ " where papp.vanaf > now()"
		+ ")"
		+ "  select COUNT(*)"
		+ "  from time_table"
		+ "  where start_time < :startTijd or end_time > :eindTijd")
	List<Long> countColonIntakeAfsprakenInNacht(@Param("startTijd") LocalTime startTijd, @Param("eindTijd") LocalTime eindTijd);

	@Query(nativeQuery = true, value = "select count(afs.id) as count"
		+ " from colon.afspraakslot  afs"
		+ " inner join colon.tijdslot ts on afs.id = ts.id"
		+ " where ts.vanaf > now() and DATE_PART('DOW', ts.vanaf) = :dag")
	List<Long> countColonIntakeAfsprakenOpDag(@Param("dag") int dag);
}
