package nl.rivm.screenit.repository.mamma;

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

import java.util.Date;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.persistence.Tuple;

import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;

public interface MammaOnderzoekRepository extends BaseJpaRepository<MammaOnderzoek>
{
	@Query("SELECT m.afgerondDoor.id as id, COUNT(m) as count " +
		"FROM MammaAfspraak a " +
		"JOIN a.onderzoek o " +
		"JOIN o.mammografie m " +
		"JOIN a.standplaatsPeriode sp " +
		"JOIN sp.screeningsEenheid se " +
		"WHERE se.code = :seCode " +
		"AND a.vanaf BETWEEN :beginDatum AND :eindDatum " +
		"AND m.afgerondDoor IS NOT NULL " +
		"GROUP BY m.afgerondDoor.id")
	Stream<Tuple> readOnderzochtVanSeOpWerkdag(Date beginDatum,
		Date eindDatum,
		String seCode);

	@Query("SELECT m.afgerondDoor.id as id, COUNT(m) as count " +
		"FROM MammaAfspraak a " +
		"JOIN a.onderzoek o " +
		"JOIN o.mammografie m " +
		"JOIN a.standplaatsPeriode sp " +
		"JOIN sp.screeningsEenheid se " +
		"WHERE se.code = :seCode " +
		"AND a.vanaf BETWEEN :beginDatum AND :eindDatum " +
		"AND o.status = :status " +
		"AND m.afgerondDoor IS NOT NULL " +
		"GROUP BY m.afgerondDoor.id")
	Stream<Tuple> readOnderzoekStatusCountVanSeOpWerkdag(Date beginDatum,
		Date eindDatum,
		String seCode,
		MammaOnderzoekStatus status);

	@Query("SELECT s.afgerondDoor.id as id, COUNT(s) as count " +
		"FROM MammaAfspraak a " +
		"JOIN a.onderzoek o " +
		"LEFT JOIN o.signaleren s " +
		"JOIN a.standplaatsPeriode sp " +
		"JOIN sp.screeningsEenheid se " +
		"WHERE se.code = :seCode " +
		"AND a.vanaf BETWEEN :beginDatum AND :eindDatum " +
		"AND o.signaleren IS NOT NULL " +
		"AND s.afgerondDoor IS NOT NULL " +
		"AND s.heeftAfwijkingen = true " +
		"GROUP BY s.afgerondDoor.id")
	Stream<Tuple> readAfwijkingenVanSeOpWerkdag(Date beginDatum,
		Date eindDatum,
		String seCode);

	default Map<Long, Integer> readOnderzochtVanSeOpWerkdagToMap(Date beginDatum, Date eindDatum, String seCode)
	{
		return readOnderzochtVanSeOpWerkdag(beginDatum, eindDatum, seCode)
			.collect(Collectors.toMap(
				tuple -> tuple.get("id", Long.class),
				tuple -> tuple.get("count", Long.class).intValue())
			);
	}

	default Map<Long, Integer> readOnderzoekStatusCountVanSeOpWerkdagtoMap(Date beginDatum, Date eindDatum, String seCode, MammaOnderzoekStatus onderzoekStatus)
	{
		return readOnderzoekStatusCountVanSeOpWerkdag(beginDatum, eindDatum, seCode, onderzoekStatus)
			.collect(Collectors.toMap(
				tuple -> tuple.get("id", Long.class),
				tuple -> tuple.get("count", Long.class).intValue())
			);
	}

	default Map<Long, Integer> readAfwijkingenVanSeOpWerkdagToMap(Date beginDatum, Date eindDatum, String seCode)
	{
		return readAfwijkingenVanSeOpWerkdag(beginDatum, eindDatum, seCode)
			.collect(Collectors.toMap(
				tuple -> tuple.get("id", Long.class),
				tuple -> tuple.get("count", Long.class).intValue())
			);
	}
}
