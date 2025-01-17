package nl.rivm.screenit.main.service.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.List;

import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;

import org.springframework.data.domain.Sort;

public interface MammaAdhocMeekijkverzoekService
{
	List<MammaAdhocMeekijkverzoek> find(Collection<MammaScreeningsEenheid> screeningsEenheden, MammaVisitatieOnderzoekStatus status, long first, long count, Sort sort);

	long count(Collection<MammaScreeningsEenheid> screeningsEenheden, MammaVisitatieOnderzoekStatus status);

	long countAantalGezienByScreeningsEenheden(Collection<MammaScreeningsEenheid> screeningsEenheden);

	long countAantalGezienByIds(Collection<Long> ids);
}