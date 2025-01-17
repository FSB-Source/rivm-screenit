package nl.rivm.screenit.specification.mamma;

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

import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.filterScreeningsEenheid;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaAdhocMeekijkverzoekSpecification
{
	public static Specification<MammaAdhocMeekijkverzoek> filterOpScreeningsEenheid(Collection<MammaScreeningsEenheid> screeningsEenheden)
	{
		return filterScreeningsEenheid(screeningsEenheden).with(MammaAdhocMeekijkverzoek_.onderzoek);
	}

	public static Specification<MammaAdhocMeekijkverzoek> heeftStatus(MammaVisitatieOnderzoekStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaAdhocMeekijkverzoek_.status), status);
	}

	public static Specification<MammaAdhocMeekijkverzoek> filterOpStatus(MammaVisitatieOnderzoekStatus status)
	{
		return skipWhenNull(status, heeftStatus(status));
	}
}
