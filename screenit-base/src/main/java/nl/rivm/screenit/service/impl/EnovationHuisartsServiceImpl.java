package nl.rivm.screenit.service.impl;

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

import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.EnovationHuisarts_;
import nl.rivm.screenit.repository.algemeen.EnovationHuisartsRepository;
import nl.rivm.screenit.repository.colon.ColonScreeningRondeRepository;
import nl.rivm.screenit.service.EnovationHuisartsService;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.algemeen.EnovationHuisartsSpecification.filterAdres;
import static nl.rivm.screenit.specification.algemeen.EnovationHuisartsSpecification.filterNaam;
import static nl.rivm.screenit.specification.algemeen.EnovationHuisartsSpecification.isVerwijderd;

@Service
@Slf4j
public class EnovationHuisartsServiceImpl implements EnovationHuisartsService
{
	@Autowired
	private EnovationHuisartsRepository huisartsRepository;

	@Autowired
	private ColonScreeningRondeRepository colonScreeningRondeRepository;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional
	public void saveOrUpdate(EnovationHuisarts huisarts)
	{
		huisartsRepository.save(huisarts);
	}

	@Override
	public EnovationHuisarts getHuisartsByKlantnummer(String klantnummer)
	{
		return huisartsRepository.findOneByKlantnummer(klantnummer).orElse(null);
	}

	@Override
	public List<EnovationHuisarts> zoekHuisartsen(EnovationHuisarts zoekObject, PageRequest pageRequest)
	{
		return huisartsRepository.findAll(getHuisartsenSpecification(zoekObject), pageRequest).toList();
	}

	@Override
	public int valideerKlantnummers(List<String> klantnummers)
	{
		var activeKlantnummers = getKlantnummersVanAlleActiveHuisartsen();
		activeKlantnummers.removeAll(klantnummers);
		if (!activeKlantnummers.isEmpty())
		{
			LOG.info("{} inactieve huisartsen worden 'verwijderd'", activeKlantnummers.size());
			verwijderHuisartsen(activeKlantnummers);
		}
		return activeKlantnummers.size();
	}

	private void verwijderHuisartsen(List<String> klantnummersVanTeVerwijderenHuisartsen)
	{
		huisartsRepository.markeerHuisartsenAlsVerwijderd(currentDateSupplier.getDate(), klantnummersVanTeVerwijderenHuisartsen);
	}

	private List<String> getKlantnummersVanAlleActiveHuisartsen()
	{
		return huisartsRepository.findWith(isVerwijderd(false), String.class, q -> q.projection((cb, r) -> r.get(EnovationHuisarts_.klantnummer))).all();
	}

	@Override
	public EnovationHuisarts getHuisartsByAgb(String agbCode)
	{
		return huisartsRepository.findOneByHuisartsAgbAndVerwijderdFalse(agbCode).orElse(null);
	}

	@Override
	public Specification<EnovationHuisarts> getHuisartsenSpecification(EnovationHuisarts filter)
	{
		return filterNaam(filter.getAchternaam())
			.and(filterAdres(filter.getAdres()))
			.and(isVerwijderd(false));
	}

	@Override
	public void verwijderdeHuisartsenOntkoppelen()
	{
		colonScreeningRondeRepository.maakVerwijderdeHuisartsenLosVanScreeningRondes();
	}

}
