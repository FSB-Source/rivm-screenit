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

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakReserveringView;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaAfspraakReservering;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface MammaAfspraakReserveringRepository extends BaseJpaRepository<MammaAfspraakReservering>
{
	@Query(nativeQuery = true, value =
		"select ar.capaciteit_blok as capaciteitBlokId, ar.vanaf, ar.opkomstkans, d.doelgroep , d.eerste_onderzoek as eersteOnderzoek, t.id as tehuisId "
			+ "from mamma.afspraak_reservering ar "
			+ "join gedeeld.pat_patient pp on pp.id = ar.client "
			+ "join mamma.dossier d on pp.mamma_dossier = d.id  "
			+ "left join mamma.tehuis t on d.tehuis = t.id "
			+ "where "
			+ "ar.aangemaakt_op > :ophaalMoment "
			+ "and ar.capaciteit_blok in :capaciteitBlokIds "
			+ "and (:clientId is null or ar.client <> :clientId)")
	List<MammaAfspraakReserveringView> haalReserveringenOpVoorCapaciteitsblokken(@Param("ophaalMoment") LocalDateTime ophaalMoment,
		@Param("capaciteitBlokIds") Collection<Long> capaciteitBlokIds, @Param("clientId") Long clientId);

	void deleteAllByClient(Client client);

	void deleteAllByMedewerker(InstellingGebruiker medewerker);

	void deleteAllByAangemaaktOpBefore(LocalDateTime moment);

	Optional<MammaAfspraakReservering> findByClient(Client client);
}
