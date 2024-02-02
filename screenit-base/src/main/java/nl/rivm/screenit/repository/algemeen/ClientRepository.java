package nl.rivm.screenit.repository.algemeen;

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

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.domain.Specification;

public interface ClientRepository extends BaseJpaRepository<Client>
{

	static Specification<Client> baseSpecification()
	{
		return ((r, q, cb) -> cb.isNotNull(r));
	}

	static Specification<Client> bsnEquals(String input)
	{
		return ((r, q, cb) -> cb.equal(r.get(Client_.persoon).get(GbaPersoon_.bsn), input));
	}

	static Specification<Client> gbaStatusEquals(GbaStatus status)
	{
		return ((r, q, cb) -> cb.equal(r.get(Client_.gbaStatus), status));
	}

	static Specification<Client> gbaStatusNotEquals(GbaStatus status)
	{
		return ((r, q, cb) -> cb.notEqual(r.get(Client_.gbaStatus), status));
	}

}
