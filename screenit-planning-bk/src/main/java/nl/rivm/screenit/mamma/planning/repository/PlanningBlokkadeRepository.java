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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.mamma.planning.repository.projectie.BlokkadeProjectie;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;

public interface PlanningBlokkadeRepository extends BaseJpaRepository<MammaBlokkade>
{
	@Query("""
		SELECT b.id                   as id,
			   b.type                 as type,
			   b.standplaats.id       as standplaatsId,
			   b.screeningsEenheid.id as screeningsEenheidId,
			   b.regio.id             as screeningOrganisatieId,
			   b.vanaf                as vanaf,
			   b.totEnMet             as totEnMet
		FROM MammaBlokkade b
		WHERE b.actief = true
			AND b.totEnMet >= :plannenVanafDatum
			AND b.vanaf <= :plannenTotEnMetDatum""")
	List<BlokkadeProjectie> findActieveBlokkadesVoorConceptmodel(Date plannenVanafDatum, Date plannenTotEnMetDatum);
}
