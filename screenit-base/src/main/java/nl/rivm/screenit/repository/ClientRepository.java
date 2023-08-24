package nl.rivm.screenit.repository;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Persoon_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

@Repository
public interface ClientRepository extends JpaRepository<Client, Long>, JpaSpecificationExecutor<Client>
{

	static Specification<Client> baseSpecification()
	{
		return ((r, q, cb) -> cb.isNotNull(r));
	}

	static Specification<Client> bsnEquals(String input)
	{
		return ((r, q, cb) -> cb.equal(r.get("persoon").get(Persoon_.BSN), input));
	}

	static Specification<Client> gbaStatusEquals(GbaStatus status)
	{
		return ((r, q, cb) -> cb.equal(r.get(Client_.GBA_STATUS), status));
	}

	static Specification<Client> gbaStatusNotEquals(GbaStatus status)
	{
		return ((r, q, cb) -> cb.notEqual(r.get(Client_.GBA_STATUS), status));
	}

}
