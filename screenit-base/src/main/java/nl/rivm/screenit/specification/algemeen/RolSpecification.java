package nl.rivm.screenit.specification.algemeen;

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

import java.util.Collection;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.Rol_;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class RolSpecification
{
	public static ExtendedSpecification<Rol> isActief(boolean actief)
	{
		return (r, q, cb) -> cb.equal(r.get(Rol_.actief), actief);
	}

	public static Specification<Rol> filterBevolkingsonderzoek(Collection<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		return skipWhenEmpty(bevolkingsonderzoeken, (r, q, cb) -> r.join(Rol_.bevolkingsonderzoeken).in(bevolkingsonderzoeken));
	}
}
