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

import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.model.overeenkomsten.OvereenkomstType;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst_;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class OvereenkomstSpecification
{
	public static Specification<Overeenkomst> filterActief(Boolean actief)
	{
		return skipWhenNull(actief, (r, q, cb) -> cb.equal(r.get(Overeenkomst_.actief), actief));
	}

	public static Specification<Overeenkomst> filterOrganisatieType(OrganisatieType organisatieType)
	{
		return skipWhenNull(organisatieType, (r, q, cb) -> cb.equal(r.get(Overeenkomst_.organisatieType), organisatieType));
	}

	public static Specification<Overeenkomst> isActief()
	{
		return (r, q, cb) -> cb.isTrue(r.get(Overeenkomst_.actief));
	}

	public static Specification<Overeenkomst> heeftOvereenkomstIn(List<OvereenkomstType> overeenkomsten)
	{
		return (r, q, cb) -> r.get(Overeenkomst_.overeenkomst).in(overeenkomsten);
	}

	public static Specification<Overeenkomst> heeftOvereenkomstType(OvereenkomstType type)
	{
		return (r, q, cb) -> cb.equal(r.get(Overeenkomst_.overeenkomst), type);
	}
}
