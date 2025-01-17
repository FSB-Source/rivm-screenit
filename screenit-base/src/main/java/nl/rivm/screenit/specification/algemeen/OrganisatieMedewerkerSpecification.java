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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruiker_;
import nl.rivm.screenit.specification.ExtendedSpecification;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class OrganisatieMedewerkerSpecification
{
	public static ExtendedSpecification<InstellingGebruiker> isActief()
	{
		return (r, q, cb) -> cb.isTrue(r.get(InstellingGebruiker_.actief));
	}

	public static ExtendedSpecification<InstellingGebruiker> heeftInstelling(Instelling instelling)
	{
		return (r, q, cb) -> cb.equal(r.get(InstellingGebruiker_.organisatie), instelling);
	}

	public static ExtendedSpecification<InstellingGebruiker> heeftMedewerker(Gebruiker medewerker)
	{
		return (r, q, cb) -> cb.equal(r.get(InstellingGebruiker_.medewerker), medewerker);
	}

	public static ExtendedSpecification<InstellingGebruiker> filterActief(Boolean actief)
	{
		return skipWhenNullExtended(actief, (r, q, cb) -> cb.equal(r.get(InstellingGebruiker_.actief), actief));
	}
}
