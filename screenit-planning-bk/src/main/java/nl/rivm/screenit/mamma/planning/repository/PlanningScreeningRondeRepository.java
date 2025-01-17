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

import java.time.LocalDate;
import java.util.Date;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.persistence.Tuple;

import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;

import static nl.rivm.screenit.util.DateUtil.toLocalDate;

public interface PlanningScreeningRondeRepository extends BaseJpaRepository<MammaScreeningRonde>
{
	@Query("""
		SELECT dossier.id AS id, MAX(ronde.creatieDatum) AS datum
		FROM MammaScreeningRonde ronde
			JOIN ronde.dossier dossier
		WHERE ronde.id <> dossier.laatsteScreeningRonde.id
		GROUP BY dossier.id""")
	Stream<Tuple> findVorigeScreeningRondeDatumPerDossier();

	default Map<Long, LocalDate> findVorigeScreeningRondeDatumPerDossierToMap()
	{
		return findVorigeScreeningRondeDatumPerDossier()
			.collect(Collectors.toMap(
				tuple -> tuple.get("id", Long.class),
				tuple -> toLocalDate(tuple.get("datum", Date.class)))
			);
	}
}
