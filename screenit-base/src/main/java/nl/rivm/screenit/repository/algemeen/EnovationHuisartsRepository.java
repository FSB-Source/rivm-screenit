package nl.rivm.screenit.repository.algemeen;

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

import java.util.Date;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface EnovationHuisartsRepository extends BaseJpaRepository<EnovationHuisarts>
{
	Optional<EnovationHuisarts> findOneByKlantnummer(String klantnummer);

	Optional<EnovationHuisarts> findOneByHuisartsAgbAndVerwijderdFalse(String agbCode);

	@Modifying
	@Query("update EnovationHuisarts set verwijderd = true, gewijzigd = :date where klantnummer in :klantnummersVanTeVerwijderenHuisartsen and verwijderd = false")
	void markeerHuisartsenAlsVerwijderd(Date date, List<String> klantnummersVanTeVerwijderenHuisartsen);
}
