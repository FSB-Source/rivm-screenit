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

import java.util.Collection;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.repository.projectie.AfspraakProjectie;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;

import static nl.rivm.screenit.util.DateUtil.toUtilDate;

public interface PlanningAfspraakRepository extends BaseJpaRepository<MammaAfspraak>
{
	@Query("""
		SELECT c.id AS clientId, se.id AS screeningsEenheidId, a.vanaf AS afspraakMoment
		FROM MammaMammografie m
			JOIN m.onderzoek o
			JOIN o.afspraak a
			JOIN a.standplaatsPeriode sp
			JOIN sp.screeningsEenheid se
			JOIN a.uitnodiging u
			JOIN u.screeningRonde sr
			JOIN sr.dossier d
			JOIN d.client c
		WHERE a.vanaf < :prognoseVanafDatum
			AND sp.id IN :teLezenStandplaatsPeriodeIds
			AND a.vanaf >= :plannenVanafDatum""")
	List<AfspraakProjectie> findAfsprakenMetGebruikteCapaciteitInternal(Collection<Long> teLezenStandplaatsPeriodeIds, Date prognoseVanafDatum, Date plannenVanafDatum);

	default List<AfspraakProjectie> findAfsprakenMetGebruikteCapaciteit(Collection<Long> teLezenStandplaatsPeriodeIds)
	{
		return findAfsprakenMetGebruikteCapaciteitInternal(teLezenStandplaatsPeriodeIds,
			toUtilDate(PlanningConstanten.prognoseVanafDatum),
			toUtilDate(PlanningConstanten.plannenVanafDatum));
	}

}
