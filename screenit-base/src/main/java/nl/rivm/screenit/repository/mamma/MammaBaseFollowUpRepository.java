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

import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface MammaBaseFollowUpRepository extends BaseJpaRepository<MammaFollowUpRadiologieVerslag>
{
	@Query("""
			SELECT COUNT(d) > 0 FROM MammaDossier d
			JOIN d.laatsteBeoordelingMetUitslag b
			JOIN b.onderzoek o
			JOIN o.afspraak a
			JOIN a.uitnodiging u
			JOIN u.screeningRonde sr
			LEFT JOIN sr.followUpVerslagen fuv ON fuv.type = nl.rivm.screenit.model.berichten.enums.VerslagType.MAMMA_PA_FOLLOW_UP
			LEFT JOIN sr.followUpRadiologieVerslagen furv
			WHERE d.id = :dossierId
			AND (
				fuv.id IS NOT NULL AND
				fuv.status = nl.rivm.screenit.model.berichten.enums.VerslagStatus.AFGEROND
				AND (sr.followUpConclusieStatusGewijzigdOp IS NULL OR fuv.datumVerwerkt > sr.followUpConclusieStatusGewijzigdOp)
				OR (
					furv.id IS NOT NULL
					AND furv.ingevoerdDoor IS NOT NULL
					AND sr.followUpConclusieStatus IS NULL
					AND sr.id NOT IN (
						SELECT sr2.id FROM MammaFollowUpRadiologieVerslag furv2
						JOIN furv2.screeningRonde sr2
						WHERE furv2.pathologieUitgevoerd = true
						AND sr2.dossier.id = :dossierId
						AND furv2.paVerslagNietTeVerwachten IS NULL
						)
				   )
				)
		""")
	boolean heeftOpenstaandeFollowUpConclusie(@Param("dossierId") Long dossierId);
}
