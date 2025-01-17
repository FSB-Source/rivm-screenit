package nl.rivm.screenit.main.service.mamma.impl;

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

import nl.rivm.screenit.main.service.mamma.MammaAdhocMeekijkverzoekService;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;
import nl.rivm.screenit.repository.mamma.MammaAdhocMeekijkverzoekRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus.GEZIEN;
import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftIdIn;
import static nl.rivm.screenit.specification.mamma.MammaAdhocMeekijkverzoekSpecification.filterOpScreeningsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaAdhocMeekijkverzoekSpecification.filterOpStatus;
import static nl.rivm.screenit.specification.mamma.MammaAdhocMeekijkverzoekSpecification.heeftStatus;

@Service
public class MammaAdhocMeekijkverzoekServiceImpl implements MammaAdhocMeekijkverzoekService
{
	@Autowired
	private MammaAdhocMeekijkverzoekRepository adhocMeekijkverzoekRepository;

	@Override
	public List<MammaAdhocMeekijkverzoek> find(Collection<MammaScreeningsEenheid> screeningsEenheden, MammaVisitatieOnderzoekStatus status, long first, long count, Sort sort)
	{
		return adhocMeekijkverzoekRepository.findWith(specification(screeningsEenheden, status), q -> q.sortBy(sort)).all(first, count);
	}

	@Override
	public long count(Collection<MammaScreeningsEenheid> screeningsEenheden, MammaVisitatieOnderzoekStatus status)
	{
		return adhocMeekijkverzoekRepository.count(specification(screeningsEenheden, status));
	}

	@Override
	public long countAantalGezienByScreeningsEenheden(Collection<MammaScreeningsEenheid> screeningsEenheden)
	{
		return adhocMeekijkverzoekRepository.count(specification(screeningsEenheden, GEZIEN));
	}

	@Override
	public long countAantalGezienByIds(Collection<Long> ids)
	{
		return adhocMeekijkverzoekRepository.count(heeftStatus(GEZIEN).and(heeftIdIn(ids)));
	}

	private Specification<MammaAdhocMeekijkverzoek> specification(Collection<MammaScreeningsEenheid> screeningsEenheden, MammaVisitatieOnderzoekStatus status)
	{
		return filterOpScreeningsEenheid(screeningsEenheden).and(filterOpStatus(status));
	}
}
