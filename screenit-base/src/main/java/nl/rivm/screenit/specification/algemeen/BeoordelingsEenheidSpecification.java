package nl.rivm.screenit.specification.algemeen;

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

import java.util.Collection;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmptyExtended;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class BeoordelingsEenheidSpecification
{
	public static Specification<BeoordelingsEenheid> isActief(boolean actief)
	{
		return (r, q, cb) -> cb.equal(r.get(Instelling_.actief), actief);
	}

	public static ExtendedSpecification<BeoordelingsEenheid> heeftScreeningOrganisatie(Instelling screeningOrganisatie)
	{
		return (r, q, cb) ->
		{
			var centraleEenheidJoin = join(r, Instelling_.parent);
			return cb.equal(centraleEenheidJoin.get(Instelling_.regio), screeningOrganisatie);
		};
	}

	public static ExtendedSpecification<BeoordelingsEenheid> filterOpScreeningOrganisatie(Instelling screeningOrganisatie)
	{
		return SpecificationUtil.skipWhenNullExtended(screeningOrganisatie, heeftScreeningOrganisatie(screeningOrganisatie));
	}

	public static ExtendedSpecification<BeoordelingsEenheid> heeftCentraleEenheidIn(Collection<CentraleEenheid> centraleEenheden)
	{
		return (r, q, cb) -> r.get(Instelling_.parent).in(centraleEenheden);
	}

	public static ExtendedSpecification<BeoordelingsEenheid> filterCentraleEenheid(Collection<CentraleEenheid> centraleEenheden)
	{
		return skipWhenEmptyExtended(centraleEenheden, heeftCentraleEenheidIn(centraleEenheden));
	}
}
