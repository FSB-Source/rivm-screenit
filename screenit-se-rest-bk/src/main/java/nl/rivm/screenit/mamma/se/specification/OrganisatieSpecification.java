package nl.rivm.screenit.mamma.se.specification;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.OrganisatieType;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class OrganisatieSpecification
{
	public static Specification<Instelling> heeftSubInstellingVanType(List<OrganisatieType> organisatieTypes)
	{
		return (r, q, cb) ->
		{
			var afdelingJoin = join(r, Instelling_.children);
			return afdelingJoin.get(Instelling_.organisatieType).in(organisatieTypes);
		};
	}

	public static Specification<Instelling> isZorgInstelling()
	{
		return (r, q, cb) -> cb.equal(r.get(Instelling_.organisatieType), OrganisatieType.ZORGINSTELLING);
	}
}
