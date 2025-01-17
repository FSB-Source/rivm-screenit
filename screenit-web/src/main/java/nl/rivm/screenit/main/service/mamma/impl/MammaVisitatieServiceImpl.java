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

import java.time.LocalDate;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaVisitatieService;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaVisitatie;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderdeel;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieStatus;
import nl.rivm.screenit.repository.mamma.MammaVisitatieOnderzoekRepository;
import nl.rivm.screenit.repository.mamma.MammaVisitatieRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus.GEZIEN;
import static nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus.NIET_GEZIEN;
import static nl.rivm.screenit.specification.mamma.MammaVisitatieOnderzoekSpecification.heeftBeoordeling;
import static nl.rivm.screenit.specification.mamma.MammaVisitatieOnderzoekSpecification.heeftOnderdeel;
import static nl.rivm.screenit.specification.mamma.MammaVisitatieOnderzoekSpecification.heeftStatus;
import static nl.rivm.screenit.specification.mamma.MammaVisitatieOnderzoekSpecification.heeftVisitatie;
import static nl.rivm.screenit.specification.mamma.MammaVisitatieSpecification.filterOpAfgerondOpVanaf;
import static nl.rivm.screenit.specification.mamma.MammaVisitatieSpecification.filterOpScreeningsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaVisitatieSpecification.filterOpStatus;

@Service
public class MammaVisitatieServiceImpl implements MammaVisitatieService
{
	@Autowired
	private MammaVisitatieOnderzoekRepository visitatieOnderzoekRepository;

	@Autowired
	private MammaVisitatieRepository visitatieRepository;

	@Override
	public List<MammaVisitatieOnderzoek> zoekVisitatieOnderzoeken(MammaVisitatieOnderdeel onderdeel, MammaVisitatie visitatie, long first, long count, Sort sort)
	{
		return visitatieOnderzoekRepository.findWith(visitatieOnderzoekSpecification(onderdeel, visitatie), q -> q.sortBy(sort)).all(first, count);
	}

	@Override
	public long countVisitatieOnderzoeken(MammaVisitatieOnderdeel onderdeel, MammaVisitatie visitatie)
	{
		return visitatieOnderzoekRepository.count(visitatieOnderzoekSpecification(onderdeel, visitatie));
	}

	@Override
	public List<MammaVisitatie> zoekVisitaties(LocalDate vanaf, List<MammaScreeningsEenheid> screeningsEenheden, List<MammaVisitatieStatus> statussen, long first, long count,
		Sort sort)
	{
		return visitatieRepository.findWith(visitatieSpecification(vanaf, screeningsEenheden, statussen), q -> q.sortBy(sort)).all(first, count);
	}

	@Override
	public long countVisitaties(LocalDate vanaf, List<MammaScreeningsEenheid> screeningsEenheden, List<MammaVisitatieStatus> statussen)
	{
		return visitatieRepository.count(visitatieSpecification(vanaf, screeningsEenheden, statussen));
	}

	@Override
	public boolean isBeoordelingInVisitatieOnderdeel(MammaBeoordeling beoordeling, MammaVisitatie visitatie, MammaVisitatieOnderdeel onderdeel)
	{
		return visitatieOnderzoekRepository.exists(heeftBeoordeling(beoordeling).and(heeftVisitatie(visitatie)).and(heeftOnderdeel(onderdeel)));
	}

	@Override
	public long countAantalGezien(MammaVisitatie visitatie, MammaVisitatieOnderdeel onderdeel)
	{
		return visitatieOnderzoekRepository.count(heeftVisitatie(visitatie).and(heeftOnderdeel(onderdeel)).and(heeftStatus(GEZIEN)));
	}

	@Override
	public boolean isAllesGezien(MammaVisitatie visitatie)
	{
		return !visitatieOnderzoekRepository.exists(heeftVisitatie(visitatie).and(heeftStatus(NIET_GEZIEN)));
	}

	@Override
	public boolean kanVisitatieAfronden(MammaVisitatie visitatie)
	{
		return isAllesGezien(visitatie) && visitatie.getAfgerondOp() == null;
	}

	private Specification<MammaVisitatieOnderzoek> visitatieOnderzoekSpecification(MammaVisitatieOnderdeel onderdeel, MammaVisitatie visitatie)
	{
		return heeftOnderdeel(onderdeel).and(heeftVisitatie(visitatie));
	}

	private Specification<MammaVisitatie> visitatieSpecification(LocalDate afgerondOpVanaf, List<MammaScreeningsEenheid> screeningsEenheden, List<MammaVisitatieStatus> statussen)
	{
		return filterOpAfgerondOpVanaf(afgerondOpVanaf)
			.and(filterOpScreeningsEenheid(screeningsEenheden))
			.and(filterOpStatus(statussen));
	}
}
